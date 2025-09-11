
;; title: crop-insurance

(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-registered (err u101))
(define-constant err-already-registered (err u102))
(define-constant err-insufficient-funds (err u103))
(define-constant err-invalid-region (err u104))
(define-constant err-invalid-crop (err u105))
(define-constant err-invalid-amount (err u106))
(define-constant err-no-policy (err u107))
(define-constant err-already-claimed (err u108))
(define-constant err-not-claimable (err u109))

(define-data-var min-premium-amount uint u1000000)
(define-data-var oracle-address principal 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM)
(define-data-var total-stx-pool uint u0)

(define-constant err-insufficient-data (err u200))
(define-constant err-invalid-risk-params (err u201))
(define-constant err-risk-too-high (err u202))
(define-constant err-transfer-not-allowed (err u203))
(define-constant err-not-policy-owner (err u204))
(define-constant err-policy-already-claimed (err u205))
(define-constant err-pool-not-found (err u206))
(define-constant err-already-in-pool (err u207))
(define-constant err-not-in-pool (err u208))
(define-constant err-pool-full (err u209))
(define-constant err-insufficient-votes (err u210))
(define-constant err-voting-closed (err u211))
(define-constant err-already-voted (err u212))

(define-data-var base-premium-rate uint u100)
(define-data-var max-risk-multiplier uint u500)
(define-data-var min-data-points uint u5)

;; Cooperative pool constants
(define-constant max-pool-size u10)
(define-constant min-pool-size u3)
(define-constant pool-discount-rate u15)
(define-constant voting-period-blocks u1440)


(define-map regions uint {
  name: (string-ascii 20),
  active: bool
})

(define-map crops uint {
  name: (string-ascii 20),
  active: bool
})

(define-map farmers principal {
  registered: bool,
  total-premiums-paid: uint,
  total-claims-received: uint
})

(define-map beneficiaries principal principal)

(define-map policies 
  { farmer: principal, policy-id: uint } 
  {
    region-id: uint,
    crop-id: uint,
    premium-amount: uint,
    coverage-amount: uint,
    start-block: uint,
    end-block: uint,
    claimed: bool
  }
)

(define-map weather-data 
  { region-id: uint, stacks-block-height: uint } 
  {
    rainfall: uint,
    temperature: uint,
    drought-index: uint,
    timestamp: uint
  }
)

(define-map policy-counter principal uint)

(define-map claims 
  { farmer: principal, policy-id: uint } 
  {
    amount: uint,
    stacks-block-height: uint,
    weather-data-ref: { region-id: uint, stacks-block-height: uint }
  }
)

(define-map policy-transfer-requests
  { from-farmer: principal, policy-id: uint }
  {
    to-farmer: principal,
    transfer-price: uint,
    expires-at-block: uint,
    active: bool
  }
)

;; Cooperative pool data structures
(define-map cooperative-pools
  uint
  {
    name: (string-ascii 30),
    creator: principal,
    region-id: uint,
    crop-id: uint,
    members: (list 10 principal),
    member-count: uint,
    total-premium-pool: uint,
    active: bool,
    created-block: uint
  }
)

(define-map pool-membership
  principal
  {
    pool-id: uint,
    joined-block: uint,
    contribution: uint
  }
)

(define-map pool-claim-votes
  { pool-id: uint, claim-id: uint, voter: principal }
  {
    vote: bool,
    voted-block: uint
  }
)

(define-map pool-claims
  { pool-id: uint, claim-id: uint }
  {
    claimer: principal,
    amount: uint,
    policy-id: uint,
    votes-for: uint,
    votes-against: uint,
    voting-ends: uint,
    executed: bool,
    created-block: uint
  }
)

(define-data-var pool-counter uint u0)
(define-data-var pool-claim-counter uint u0)

(define-read-only (get-farmer-info (farmer principal))
  (default-to 
    { registered: false, total-premiums-paid: u0, total-claims-received: u0 }
    (map-get? farmers farmer)
  )
)

(define-read-only (get-policy (farmer principal) (policy-id uint))
  (map-get? policies { farmer: farmer, policy-id: policy-id })
)

(define-read-only (get-weather-data (region-id uint) (stacks-block-heights uint))
  (map-get? weather-data { region-id: region-id, stacks-block-height: stacks-block-height })
)

(define-read-only (get-region (region-id uint))
  (map-get? regions region-id)
)

(define-read-only (get-crop (crop-id uint))
  (map-get? crops crop-id)
)

(define-read-only (get-claim (farmer principal) (policy-id uint))
  (map-get? claims { farmer: farmer, policy-id: policy-id })
)

(define-read-only (get-next-policy-id (farmer principal))
  (default-to u1 (map-get? policy-counter farmer))
)

(define-read-only (get-transfer-request (from-farmer principal) (policy-id uint))
  (map-get? policy-transfer-requests { from-farmer: from-farmer, policy-id: policy-id })
)

;; Cooperative pool read-only functions
(define-read-only (get-cooperative-pool (pool-id uint))
  (map-get? cooperative-pools pool-id)
)

(define-read-only (get-pool-membership (farmer principal))
  (map-get? pool-membership farmer)
)

(define-read-only (get-pool-claim (pool-id uint) (claim-id uint))
  (map-get? pool-claims { pool-id: pool-id, claim-id: claim-id })
)

(define-read-only (get-next-pool-id)
  (+ (var-get pool-counter) u1)
)

(define-read-only (get-beneficiary (farmer principal))
  (map-get? beneficiaries farmer)
)

(define-public (set-beneficiary (delegate principal))
  (let ((farmer-info (get-farmer-info tx-sender)))
    (asserts! (get registered farmer-info) err-not-registered)
    (map-set beneficiaries tx-sender delegate)
    (ok true)
  )
)

(define-public (register-farmer)
  (let ((farmer-info (get-farmer-info tx-sender)))
    (if (get registered farmer-info)
      err-already-registered
      (begin
        (map-set farmers tx-sender {
          registered: true,
          total-premiums-paid: u0,
          total-claims-received: u0
        })
        (ok true)
      )
    )
  )
)

(define-public (add-region (region-id uint) (name (string-ascii 20)))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set regions region-id {
      name: name,
      active: true
    })
    (ok true)
  )
)

(define-public (add-crop (crop-id uint) (name (string-ascii 20)))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set crops crop-id {
      name: name,
      active: true
    })
    (ok true)
  )
)

(define-public (purchase-insurance (region-id uint) (crop-id uint) (premium-amount uint) (coverage-amount uint) (duration uint))
  (let (
    (farmer-info (get-farmer-info tx-sender))
    (region (get-region region-id))
    (crop (get-crop crop-id))
    (policy-id (get-next-policy-id tx-sender))
    (current-block stacks-block-height)
  )
    (asserts! (get registered farmer-info) err-not-registered)
    (asserts! (is-some region) err-invalid-region)
    (asserts! (is-some crop) err-invalid-crop)
    (asserts! (>= premium-amount (var-get min-premium-amount)) err-invalid-amount)
    (asserts! (> coverage-amount premium-amount) err-invalid-amount)
    
    (try! (stx-transfer? premium-amount tx-sender (as-contract tx-sender)))
    
    (var-set total-stx-pool (+ (var-get total-stx-pool) premium-amount))
    
    (map-set policies 
      { farmer: tx-sender, policy-id: policy-id } 
      {
        region-id: region-id,
        crop-id: crop-id,
        premium-amount: premium-amount,
        coverage-amount: coverage-amount,
        start-block: current-block,
        end-block: (+ current-block duration),
        claimed: false
      }
    )
    
    (map-set policy-counter tx-sender (+ policy-id u1))
    
    (map-set farmers tx-sender {
      registered: true,
      total-premiums-paid: (+ (get total-premiums-paid farmer-info) premium-amount),
      total-claims-received: (get total-claims-received farmer-info)
    })
    
    (ok policy-id)
  )
)

(define-public (submit-weather-data (region-id uint) (rainfall uint) (temperature uint) (drought-index uint))
  (begin
    (asserts! (is-eq tx-sender (var-get oracle-address)) err-owner-only)
    (asserts! (is-some (get-region region-id)) err-invalid-region)
    
    (map-set weather-data 
      { region-id: region-id, stacks-block-height: stacks-block-height } 
      {
        rainfall: rainfall,
        temperature: temperature,
        drought-index: drought-index,
        timestamp: (unwrap-panic (get-stacks-block-info? time stacks-block-height))
      }
    )
    
    (ok true)
  )
)

(define-public (claim-insurance (farmer principal) (policy-id uint))
  (let (
    (caller tx-sender)
    (policy (get-policy farmer policy-id))
    (farmer-info (get-farmer-info farmer))
  )
    (asserts! (is-some policy) err-no-policy)
    ;; Verify caller is either the farmer or authorized to claim on their behalf
    (asserts! (or (is-eq caller farmer)
                  (is-eq (get-beneficiary farmer) (some caller))) err-not-policy-owner)
    (let (
      (policy-data (unwrap-panic policy))
      (weather-data-entry (get-weather-data (get region-id policy-data) stacks-block-height))
    )
      (asserts! (not (get claimed policy-data)) err-already-claimed)
      (asserts! (<= stacks-block-height (get end-block policy-data)) err-not-claimable)
      (asserts! (is-some weather-data-entry) err-not-claimable)
      
      (let (
        (weather-info (unwrap-panic weather-data-entry))
        (payout-amount (calculate-payout 
                         (get drought-index weather-info) 
                         (get coverage-amount policy-data)))
      )
        (asserts! (> payout-amount u0) err-not-claimable)
        (asserts! (<= payout-amount (var-get total-stx-pool)) err-insufficient-funds)
        
        (try! (as-contract (stx-transfer? payout-amount (as-contract tx-sender) farmer)))
        
        (var-set total-stx-pool (- (var-get total-stx-pool) payout-amount))
        
        (map-set policies 
          { farmer: farmer, policy-id: policy-id } 
          (merge policy-data { claimed: true })
        )
        
        (map-set claims 
          { farmer: farmer, policy-id: policy-id } 
          {
            amount: payout-amount,
            stacks-block-height: stacks-block-height,
            weather-data-ref: { region-id: (get region-id policy-data), stacks-block-height: stacks-block-height }
          }
        )
        
        (map-set farmers farmer {
          registered: true,
          total-premiums-paid: (get total-premiums-paid farmer-info),
          total-claims-received: (+ (get total-claims-received farmer-info) payout-amount)
        })
        
        (ok payout-amount)
      )
    )
  )
)

(define-public (set-oracle-address (new-oracle principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set oracle-address new-oracle)
    (ok true)
  )
)

(define-public (set-min-premium (new-min-premium uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set min-premium-amount new-min-premium)
    (ok true)
  )
)

(define-read-only (calculate-payout (drought-index uint) (coverage-amount uint))
  (if (> drought-index u70)
    coverage-amount
    (if (> drought-index u50)
      (/ (* coverage-amount u75) u100)
      (if (> drought-index u30)
        (/ (* coverage-amount u50) u100)
        u0
      )
    )
  )
)



(define-constant season-discount-rate u20)

(define-map multi-season-policies 
  { farmer: principal, bundle-id: uint }
  {
    policy-ids: (list 10 uint),
    seasons: uint,
    total-premium: uint,
    active: bool
  }
)

(define-public (purchase-multi-season-insurance 
    (region-id uint) 
    (crop-id uint) 
    (premium-per-season uint) 
    (coverage-per-season uint)
    (season-count uint)
    (blocks-per-season uint))
  (let (
    (bundle-discount (/ (* premium-per-season season-discount-rate) u100))
    (discounted-premium-per-season (- premium-per-season bundle-discount))
    (total-premium (* discounted-premium-per-season season-count))
    (policy-ids (list u0))
  )
    (asserts! (>= season-count u2) err-invalid-amount)
    (asserts! (<= season-count u10) err-invalid-amount)
    
    (try! (stx-transfer? total-premium tx-sender (as-contract tx-sender)))
    
    (var-set total-stx-pool (+ (var-get total-stx-pool) total-premium))
    
    (let ((bundle-id (get-next-policy-id tx-sender)))
      (map-set multi-season-policies
        { farmer: tx-sender, bundle-id: bundle-id }
        {
          policy-ids: policy-ids,
          seasons: season-count,
          total-premium: total-premium,
          active: true
        }
      )
      (ok bundle-id)
    )
  )
)


(define-constant min-stake-amount u1000000000)
(define-constant staking-fee-percent u5)

(define-map stakers
  principal
  {
    staked-amount: uint,
    rewards-claimed: uint,
    last-reward-block: uint
  }
)

(define-data-var total-staked-amount uint u0)

(define-public (stake-tokens (amount uint))
  (let (
    (staker-info (default-to 
      { staked-amount: u0, rewards-claimed: u0, last-reward-block: stacks-block-height }
      (map-get? stakers tx-sender)))
  )
    (asserts! (>= amount min-stake-amount) err-invalid-amount)
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    (map-set stakers tx-sender {
      staked-amount: (+ (get staked-amount staker-info) amount),
      rewards-claimed: (get rewards-claimed staker-info),
      last-reward-block: stacks-block-height
    })
    
    (var-set total-staked-amount (+ (var-get total-staked-amount) amount))
    (ok true)
  )
)

(define-private (calculate-staking-reward (staked-amount uint) (blocks uint))
  (/ (* staked-amount blocks staking-fee-percent) u10000))

(define-public (claim-staking-rewards)
  (let (
    (staker-info (unwrap! (map-get? stakers tx-sender) err-not-registered))
    (blocks-elapsed (- stacks-block-height (get last-reward-block staker-info)))
    (reward-amount (calculate-staking-reward 
      (get staked-amount staker-info)
      blocks-elapsed))
  )
    (try! (as-contract (stx-transfer? reward-amount (as-contract tx-sender) tx-sender)))
    
    (map-set stakers tx-sender {
      staked-amount: (get staked-amount staker-info),
      rewards-claimed: (+ (get rewards-claimed staker-info) reward-amount),
      last-reward-block: stacks-block-height
    })
    (ok reward-amount)
  )
)



(define-map regional-risk-factors
  uint
  {
    drought-frequency: uint,
    flood-frequency: uint,
    temperature-volatility: uint,
    historical-claims-ratio: uint,
    last-updated: uint
  }
)

(define-map crop-risk-multipliers
  uint
  {
    base-multiplier: uint,
    drought-sensitivity: uint,
    flood-sensitivity: uint,
    temperature-sensitivity: uint
  }
)

(define-map historical-weather-summary
  { region-id: uint, year: uint }
  {
    avg-drought-index: uint,
    extreme-weather-events: uint,
    total-rainfall: uint,
    avg-temperature: uint,
    data-points: uint
  }
)

(define-read-only (get-regional-risk (region-id uint))
  (map-get? regional-risk-factors region-id)
)

(define-read-only (get-crop-risk-multiplier (crop-id uint))
  (map-get? crop-risk-multipliers crop-id)
)

(define-read-only (calculate-risk-score (region-id uint) (crop-id uint))
  (let (
    (regional-risk (unwrap! (get-regional-risk region-id) err-insufficient-data))
    (crop-multiplier (unwrap! (get-crop-risk-multiplier crop-id) err-insufficient-data))
  )
    (let (
      (drought-risk (* (get drought-frequency regional-risk) (get drought-sensitivity crop-multiplier)))
      (flood-risk (* (get flood-frequency regional-risk) (get flood-sensitivity crop-multiplier)))
      (temp-risk (* (get temperature-volatility regional-risk) (get temperature-sensitivity crop-multiplier)))
      (base-risk (get base-multiplier crop-multiplier))
    )
      (ok (+ base-risk (/ (+ drought-risk flood-risk temp-risk) u300)))
    )
  )
)

(define-read-only (calculate-dynamic-premium (coverage-amount uint) (region-id uint) (crop-id uint))
  (let (
    (risk-score (unwrap! (calculate-risk-score region-id crop-id) err-insufficient-data))
  )
    (asserts! (<= risk-score (var-get max-risk-multiplier)) err-risk-too-high)
    (let (
      (base-premium (/ (* coverage-amount (var-get base-premium-rate)) u10000))
      (risk-adjusted-premium (/ (* base-premium risk-score) u100))
    )
      (ok risk-adjusted-premium)
    )
  )
)

(define-public (set-regional-risk-factors 
    (region-id uint)
    (drought-freq uint)
    (flood-freq uint) 
    (temp-volatility uint)
    (claims-ratio uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set regional-risk-factors region-id {
      drought-frequency: drought-freq,
      flood-frequency: flood-freq,
      temperature-volatility: temp-volatility,
      historical-claims-ratio: claims-ratio,
      last-updated: stacks-block-height
    })
    (ok true)
  )
)

(define-public (set-crop-risk-multiplier
    (crop-id uint)
    (base-mult uint)
    (drought-sens uint)
    (flood-sens uint)
    (temp-sens uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (map-set crop-risk-multipliers crop-id {
      base-multiplier: base-mult,
      drought-sensitivity: drought-sens,
      flood-sensitivity: flood-sens,
      temperature-sensitivity: temp-sens
    })
    (ok true)
  )
)

(define-public (update-historical-data 
    (region-id uint)
    (year uint)
    (avg-drought uint)
    (extreme-events uint)
    (total-rain uint)
    (avg-temp uint)
    (data-count uint))
  (begin
    (asserts! (is-eq tx-sender (var-get oracle-address)) err-owner-only)
    (asserts! (>= data-count (var-get min-data-points)) err-insufficient-data)
    (map-set historical-weather-summary 
      { region-id: region-id, year: year }
      {
        avg-drought-index: avg-drought,
        extreme-weather-events: extreme-events,
        total-rainfall: total-rain,
        avg-temperature: avg-temp,
        data-points: data-count
      }
    )
    (ok true)
  )
)

(define-public (purchase-smart-insurance (region-id uint) (crop-id uint) (coverage-amount uint) (duration uint))
  (let (
    (farmer-info (get-farmer-info tx-sender))
    (region (get-region region-id))
    (crop (get-crop crop-id))
    (calculated-premium (unwrap! (calculate-dynamic-premium coverage-amount region-id crop-id) err-insufficient-data))
    (policy-id (get-next-policy-id tx-sender))
    (current-block stacks-block-height)
  )
    (asserts! (get registered farmer-info) err-not-registered)
    (asserts! (is-some region) err-invalid-region)
    (asserts! (is-some crop) err-invalid-crop)
    (asserts! (>= calculated-premium (var-get min-premium-amount)) err-invalid-amount)
    
    (try! (stx-transfer? calculated-premium tx-sender (as-contract tx-sender)))
    
    (var-set total-stx-pool (+ (var-get total-stx-pool) calculated-premium))
    
    (map-set policies 
      { farmer: tx-sender, policy-id: policy-id } 
      {
        region-id: region-id,
        crop-id: crop-id,
        premium-amount: calculated-premium,
        coverage-amount: coverage-amount,
        start-block: current-block,
        end-block: (+ current-block duration),
        claimed: false
      }
    )
    
    (map-set policy-counter tx-sender (+ policy-id u1))
    
    (map-set farmers tx-sender {
      registered: true,
      total-premiums-paid: (+ (get total-premiums-paid farmer-info) calculated-premium),
      total-claims-received: (get total-claims-received farmer-info)
    })
    
    (ok { policy-id: policy-id, premium-paid: calculated-premium })
  )
)

(define-public (get-premium-quote (coverage-amount uint) (region-id uint) (crop-id uint))
  (calculate-dynamic-premium coverage-amount region-id crop-id)
)

(define-public (set-base-premium-rate (new-rate uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set base-premium-rate new-rate)
    (ok true)
  )
)

(define-public (set-max-risk-multiplier (new-max uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (var-set max-risk-multiplier new-max)
    (ok true)
  )
)

(define-public (create-transfer-request (policy-id uint) (to-farmer principal) (transfer-price uint) (expiry-blocks uint))
  (let (
    (policy (get-policy tx-sender policy-id))
    (current-block stacks-block-height)
  )
    (asserts! (is-some policy) err-no-policy)
    (let ((policy-data (unwrap-panic policy)))
      (asserts! (not (get claimed policy-data)) err-policy-already-claimed)
      (asserts! (> (get end-block policy-data) current-block) err-not-claimable)
      (asserts! (get registered (get-farmer-info to-farmer)) err-not-registered)
      
      (map-set policy-transfer-requests 
        { from-farmer: tx-sender, policy-id: policy-id }
        {
          to-farmer: to-farmer,
          transfer-price: transfer-price,
          expires-at-block: (+ current-block expiry-blocks),
          active: true
        }
      )
      (ok true)
    )
  )
)

(define-public (accept-transfer-request (from-farmer principal) (policy-id uint))
  (let (
    (transfer-request (get-transfer-request from-farmer policy-id))
    (policy (get-policy from-farmer policy-id))
    (current-block stacks-block-height)
  )
    (asserts! (is-some transfer-request) err-transfer-not-allowed)
    (asserts! (is-some policy) err-no-policy)
    
    (let (
      (request-data (unwrap-panic transfer-request))
      (policy-data (unwrap-panic policy))
    )
      (asserts! (get active request-data) err-transfer-not-allowed)
      (asserts! (< current-block (get expires-at-block request-data)) err-transfer-not-allowed)
      (asserts! (is-eq tx-sender (get to-farmer request-data)) err-not-policy-owner)
      (asserts! (not (get claimed policy-data)) err-policy-already-claimed)
      
      (try! (stx-transfer? (get transfer-price request-data) tx-sender from-farmer))
      
      (map-set policies 
        { farmer: tx-sender, policy-id: policy-id }
        policy-data
      )
      
      (map-delete policies { farmer: from-farmer, policy-id: policy-id })
      
      (map-set policy-transfer-requests 
        { from-farmer: from-farmer, policy-id: policy-id }
        (merge request-data { active: false })
      )
      
      (ok true)
    )
  )
)

(define-public (cancel-transfer-request (policy-id uint))
  (let (
    (transfer-request (get-transfer-request tx-sender policy-id))
  )
    (asserts! (is-some transfer-request) err-transfer-not-allowed)
    (let ((request-data (unwrap-panic transfer-request)))
      (asserts! (get active request-data) err-transfer-not-allowed)
      
      (map-set policy-transfer-requests 
        { from-farmer: tx-sender, policy-id: policy-id }
        (merge request-data { active: false })
      )
      (ok true)
    )
  )
)

;; Cooperative insurance pool functions
(define-public (create-cooperative-pool (name (string-ascii 30)) (region-id uint) (crop-id uint))
  (let (
    (farmer-info (get-farmer-info tx-sender))
    (pool-id (get-next-pool-id))
    (current-block stacks-block-height)
  )
    (asserts! (get registered farmer-info) err-not-registered)
    (asserts! (is-some (get-region region-id)) err-invalid-region)
    (asserts! (is-some (get-crop crop-id)) err-invalid-crop)
    (asserts! (is-none (get-pool-membership tx-sender)) err-already-in-pool)
    
    ;; Create the pool with creator as first member
    (map-set cooperative-pools pool-id {
      name: name,
      creator: tx-sender,
      region-id: region-id,
      crop-id: crop-id,
      members: (list tx-sender),
      member-count: u1,
      total-premium-pool: u0,
      active: true,
      created-block: current-block
    })
    
    ;; Set creator's membership
    (map-set pool-membership tx-sender {
      pool-id: pool-id,
      joined-block: current-block,
      contribution: u0
    })
    
    (var-set pool-counter pool-id)
    (ok pool-id)
  )
)

(define-public (join-cooperative-pool (pool-id uint))
  (let (
    (farmer-info (get-farmer-info tx-sender))
    (pool-info (get-cooperative-pool pool-id))
    (current-block stacks-block-height)
  )
    (asserts! (get registered farmer-info) err-not-registered)
    (asserts! (is-some pool-info) err-pool-not-found)
    (asserts! (is-none (get-pool-membership tx-sender)) err-already-in-pool)
    
    (let ((pool-data (unwrap-panic pool-info)))
      (asserts! (get active pool-data) err-pool-not-found)
      (asserts! (< (get member-count pool-data) max-pool-size) err-pool-full)
      
      ;; Add member to pool
      (map-set cooperative-pools pool-id 
        (merge pool-data {
          members: (unwrap-panic (as-max-len? (append (get members pool-data) tx-sender) u10)),
          member-count: (+ (get member-count pool-data) u1)
        })
      )
      
      ;; Set member's pool info
      (map-set pool-membership tx-sender {
        pool-id: pool-id,
        joined-block: current-block,
        contribution: u0
      })
      
      (ok true)
    )
  )
)

(define-public (leave-cooperative-pool)
  (let (
    (membership (get-pool-membership tx-sender))
  )
    (asserts! (is-some membership) err-not-in-pool)
    (let (
      (member-data (unwrap-panic membership))
      (pool-id (get pool-id member-data))
      (pool-info (unwrap! (get-cooperative-pool pool-id) err-pool-not-found))
    )
      ;; Remove member from pool
      (map-set cooperative-pools pool-id 
        (merge pool-info {
          members: (filter remove-member (get members pool-info)),
          member-count: (- (get member-count pool-info) u1)
        })
      )
      
      ;; Remove membership record
      (map-delete pool-membership tx-sender)
      (ok true)
    )
  )
)

;; Helper function to remove member from list
(define-private (remove-member (member principal))
  (not (is-eq member tx-sender))
)

(define-public (contribute-to-pool (amount uint))
  (let (
    (membership (get-pool-membership tx-sender))
  )
    (asserts! (is-some membership) err-not-in-pool)
    (asserts! (>= amount (var-get min-premium-amount)) err-invalid-amount)
    
    (let (
      (member-data (unwrap-panic membership))
      (pool-id (get pool-id member-data))
      (pool-info (unwrap! (get-cooperative-pool pool-id) err-pool-not-found))
    )
      ;; Transfer STX to contract
      (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
      
      ;; Update pool total
      (map-set cooperative-pools pool-id 
        (merge pool-info {
          total-premium-pool: (+ (get total-premium-pool pool-info) amount)
        })
      )
      
      ;; Update member contribution
      (map-set pool-membership tx-sender 
        (merge member-data {
          contribution: (+ (get contribution member-data) amount)
        })
      )
      
      (ok true)
    )
  )
)

(define-public (submit-pool-claim (policy-id uint) (claim-amount uint))
  (let (
    (membership (get-pool-membership tx-sender))
    (policy (get-policy tx-sender policy-id))
  )
    (asserts! (is-some membership) err-not-in-pool)
    (asserts! (is-some policy) err-no-policy)
    
    (let (
      (member-data (unwrap-panic membership))
      (pool-id (get pool-id member-data))
      (policy-data (unwrap-panic policy))
      (claim-id (+ (var-get pool-claim-counter) u1))
      (current-block stacks-block-height)
    )
      (asserts! (not (get claimed policy-data)) err-already-claimed)
      (asserts! (<= claim-amount (get coverage-amount policy-data)) err-invalid-amount)
      
      ;; Create claim for voting
      (map-set pool-claims 
        { pool-id: pool-id, claim-id: claim-id }
        {
          claimer: tx-sender,
          amount: claim-amount,
          policy-id: policy-id,
          votes-for: u0,
          votes-against: u0,
          voting-ends: (+ current-block voting-period-blocks),
          executed: false,
          created-block: current-block
        }
      )
      
      (var-set pool-claim-counter claim-id)
      (ok claim-id)
    )
  )
)

(define-public (vote-on-claim (pool-id uint) (claim-id uint) (vote bool))
  (let (
    (membership (get-pool-membership tx-sender))
    (claim-info (get-pool-claim pool-id claim-id))
    (current-block stacks-block-height)
  )
    (asserts! (is-some membership) err-not-in-pool)
    (asserts! (is-some claim-info) err-pool-not-found)
    (asserts! (is-eq pool-id (get pool-id (unwrap-panic membership))) err-not-in-pool)
    
    (let ((claim-data (unwrap-panic claim-info)))
      (asserts! (< current-block (get voting-ends claim-data)) err-voting-closed)
      (asserts! (not (get executed claim-data)) err-already-claimed)
      
      ;; Check if already voted
      (asserts! (is-none (map-get? pool-claim-votes { pool-id: pool-id, claim-id: claim-id, voter: tx-sender })) 
               err-already-voted)
      
      ;; Record vote
      (map-set pool-claim-votes 
        { pool-id: pool-id, claim-id: claim-id, voter: tx-sender }
        {
          vote: vote,
          voted-block: current-block
        }
      )
      
      ;; Update claim vote counts
      (map-set pool-claims 
        { pool-id: pool-id, claim-id: claim-id }
        (if vote
          (merge claim-data { votes-for: (+ (get votes-for claim-data) u1) })
          (merge claim-data { votes-against: (+ (get votes-against claim-data) u1) })
        )
      )
      
      (ok true)
    )
  )
)

(define-public (execute-pool-claim (pool-id uint) (claim-id uint))
  (let (
    (claim-info (get-pool-claim pool-id claim-id))
    (pool-info (get-cooperative-pool pool-id))
    (current-block stacks-block-height)
  )
    (asserts! (is-some claim-info) err-pool-not-found)
    (asserts! (is-some pool-info) err-pool-not-found)
    
    (let (
      (claim-data (unwrap-panic claim-info))
      (pool-data (unwrap-panic pool-info))
    )
      (asserts! (>= current-block (get voting-ends claim-data)) err-voting-closed)
      (asserts! (not (get executed claim-data)) err-already-claimed)
      
      ;; Require majority approval (more than half)
      (let ((required-votes (/ (get member-count pool-data) u2)))
        (asserts! (> (get votes-for claim-data) required-votes) err-insufficient-votes)
        (asserts! (>= (get total-premium-pool pool-data) (get amount claim-data)) err-insufficient-funds)
        
        ;; Execute payout
        (try! (as-contract (stx-transfer? (get amount claim-data) (as-contract tx-sender) (get claimer claim-data))))
        
        ;; Update pool balance
        (map-set cooperative-pools pool-id 
          (merge pool-data {
            total-premium-pool: (- (get total-premium-pool pool-data) (get amount claim-data))
          })
        )
        
        ;; Mark claim as executed
        (map-set pool-claims 
          { pool-id: pool-id, claim-id: claim-id }
          (merge claim-data { executed: true })
        )
        
        (ok true)
      )
    )
  )
)



