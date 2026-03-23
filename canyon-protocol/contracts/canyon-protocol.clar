;; Canyon Protocol - Liquid Staking Contract
;; 
;; Core Features:
;; - Multi-asset liquid staking with cToken representation
;; - Governance token (CANF) distribution
;; - Time-weighted staking rewards
;; - Emergency unstaking mechanism
;; - Insurance fund for slashing protection

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-insufficient-balance (err u101))
(define-constant err-invalid-amount (err u102))
(define-constant err-pool-not-found (err u103))
(define-constant err-unstaking-locked (err u104))

;; Minimum staking amount (1 STX)
(define-constant min-stake-amount u1000000)

;; Emergency unstaking cooldown (144 blocks = ~24 hours)
(define-constant unstaking-cooldown u144)

;; Data Variables
(define-data-var total-staked uint u0)
(define-data-var total-canf-supply uint u0)
(define-data-var insurance-fund-balance uint u0)
(define-data-var protocol-active bool true)

;; Data Maps

;; User staking positions
(define-map user-stakes
    principal
    {
        staked-amount: uint,
        ctoken-balance: uint,
        stake-block: uint,
        rewards-earned: uint
    }
)

;; Validator pools with performance metrics
(define-map validator-pools
    uint
    {
        pool-id: uint,
        total-staked: uint,
        validator-count: uint,
        performance-score: uint,
        active: bool
    }
)

;; Governance token balances
(define-map canf-balances
    principal
    uint
)

;; Unstaking requests with cooldown
(define-map unstaking-requests
    principal
    {
        amount: uint,
        request-block: uint,
        completed: bool
    }
)

;; Read-only functions

(define-read-only (get-user-stake (user principal))
    (default-to
        {staked-amount: u0, ctoken-balance: u0, stake-block: u0, rewards-earned: u0}
        (map-get? user-stakes user)
    )
)

(define-read-only (get-canf-balance (user principal))
    (default-to u0 (map-get? canf-balances user))
)

(define-read-only (get-total-staked)
    (var-get total-staked)
)

(define-read-only (get-insurance-fund)
    (var-get insurance-fund-balance)
)

(define-read-only (get-validator-pool (pool-id uint))
    (map-get? validator-pools pool-id)
)

(define-read-only (calculate-rewards (user principal))
    (let
        (
            (stake-info (get-user-stake user))
            (staked (get staked-amount stake-info))
            (stake-duration (- block-height (get stake-block stake-info)))
        )
        ;; Simple reward calculation: 5% APY approximation
        ;; Rewards = (staked * blocks * rate) / (blocks-per-year * 100)
        (/ (* (* staked stake-duration) u5) u5256000)
    )
)

;; Public functions

;; Stake STX and receive cTokens (1:1 ratio initially)
(define-public (stake (amount uint) (pool-id uint))
    (let
        (
            (sender tx-sender)
            (current-stake (get-user-stake sender))
            (pool (unwrap! (map-get? validator-pools pool-id) err-pool-not-found))
        )
        ;; Validations
        (asserts! (var-get protocol-active) (err u105))
        (asserts! (>= amount min-stake-amount) err-invalid-amount)
        (asserts! (get active pool) err-pool-not-found)
        
        ;; Transfer STX to contract
        (try! (stx-transfer? amount sender (as-contract tx-sender)))
        
        ;; Update user stake
        (map-set user-stakes sender {
            staked-amount: (+ (get staked-amount current-stake) amount),
            ctoken-balance: (+ (get ctoken-balance current-stake) amount),
            stake-block: block-height,
            rewards-earned: (get rewards-earned current-stake)
        })
        
        ;; Update pool
        (map-set validator-pools pool-id
            (merge pool {total-staked: (+ (get total-staked pool) amount)})
        )
        
        ;; Update global state
        (var-set total-staked (+ (var-get total-staked) amount))
        
        ;; Mint CANF governance tokens (10% of staked amount)
        (let ((canf-amount (/ amount u10)))
            (map-set canf-balances sender 
                (+ (get-canf-balance sender) canf-amount))
            (var-set total-canf-supply (+ (var-get total-canf-supply) canf-amount))
        )
        
        (ok amount)
    )
)

;; Request unstaking with cooldown period
(define-public (request-unstake (amount uint))
    (let
        (
            (sender tx-sender)
            (stake-info (get-user-stake sender))
        )
        ;; Validations
        (asserts! (>= (get ctoken-balance stake-info) amount) err-insufficient-balance)
        (asserts! (> amount u0) err-invalid-amount)
        
        ;; Create unstaking request
        (map-set unstaking-requests sender {
            amount: amount,
            request-block: block-height,
            completed: false
        })
        
        (ok true)
    )
)

;; Complete unstaking after cooldown period
(define-public (complete-unstake)
    (let
        (
            (sender tx-sender)
            (request (unwrap! (map-get? unstaking-requests sender) (err u106)))
            (stake-info (get-user-stake sender))
            (amount (get amount request))
        )
        ;; Check cooldown period
        (asserts! (>= (- block-height (get request-block request)) unstaking-cooldown) 
            err-unstaking-locked)
        (asserts! (not (get completed request)) (err u107))
        
        ;; Calculate and add rewards
        (let ((rewards (calculate-rewards sender)))
            ;; Transfer STX back to user (staked amount + rewards)
            (try! (as-contract (stx-transfer? (+ amount rewards) tx-sender sender)))
            
            ;; Update user stake
            (map-set user-stakes sender {
                staked-amount: (- (get staked-amount stake-info) amount),
                ctoken-balance: (- (get ctoken-balance stake-info) amount),
                stake-block: (get stake-block stake-info),
                rewards-earned: (+ (get rewards-earned stake-info) rewards)
            })
            
            ;; Mark request as completed
            (map-set unstaking-requests sender
                (merge request {completed: true})
            )
            
            ;; Update global state
            (var-set total-staked (- (var-get total-staked) amount))
            
            (ok (+ amount rewards))
        )
    )
)

;; Emergency unstake (no cooldown, 5% penalty to insurance fund)
(define-public (emergency-unstake (amount uint))
    (let
        (
            (sender tx-sender)
            (stake-info (get-user-stake sender))
            (penalty (/ amount u20))
            (net-amount (- amount penalty))
        )
        ;; Validations
        (asserts! (>= (get ctoken-balance stake-info) amount) err-insufficient-balance)
        (asserts! (> amount u0) err-invalid-amount)
        
        ;; Transfer net amount to user
        (try! (as-contract (stx-transfer? net-amount tx-sender sender)))
        
        ;; Add penalty to insurance fund
        (var-set insurance-fund-balance (+ (var-get insurance-fund-balance) penalty))
        
        ;; Update user stake
        (map-set user-stakes sender {
            staked-amount: (- (get staked-amount stake-info) amount),
            ctoken-balance: (- (get ctoken-balance stake-info) amount),
            stake-block: (get stake-block stake-info),
            rewards-earned: (get rewards-earned stake-info)
        })
        
        ;; Update global state
        (var-set total-staked (- (var-get total-staked) amount))
        
        (ok net-amount)
    )
)

;; Claim accumulated rewards
(define-public (claim-rewards)
    (let
        (
            (sender tx-sender)
            (rewards (calculate-rewards sender))
            (stake-info (get-user-stake sender))
        )
        (asserts! (> rewards u0) (err u108))
        
        ;; Transfer rewards
        (try! (as-contract (stx-transfer? rewards tx-sender sender)))
        
        ;; Update stake info with reset block and accumulated rewards
        (map-set user-stakes sender
            (merge stake-info {
                stake-block: block-height,
                rewards-earned: (+ (get rewards-earned stake-info) rewards)
            })
        )
        
        (ok rewards)
    )
)

;; Admin functions

;; Initialize a validator pool
(define-public (create-validator-pool (pool-id uint) (validator-count uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        
        (map-set validator-pools pool-id {
            pool-id: pool-id,
            total-staked: u0,
            validator-count: validator-count,
            performance-score: u100,
            active: true
        })
        
        (ok true)
    )
)

;; Update pool performance score (0-100)
(define-public (update-pool-performance (pool-id uint) (score uint))
    (let
        (
            (pool (unwrap! (map-get? validator-pools pool-id) err-pool-not-found))
        )
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (<= score u100) (err u109))
        
        (map-set validator-pools pool-id
            (merge pool {performance-score: score})
        )
        
        (ok true)
    )
)

;; Toggle protocol active state
(define-public (set-protocol-active (active bool))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (var-set protocol-active active)
        (ok true)
    )
)

;; Fund insurance pool
(define-public (fund-insurance (amount uint))
    (begin
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        (var-set insurance-fund-balance (+ (var-get insurance-fund-balance) amount))
        (ok true)
    )
)
