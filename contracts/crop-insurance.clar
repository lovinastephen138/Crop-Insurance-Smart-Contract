
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

(define-data-var base-premium-rate uint u100)
(define-data-var max-risk-multiplier uint u500)
(define-data-var min-data-points uint u5)


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

(define-public (claim-insurance (policy-id uint))
  (let (
    (farmer tx-sender)
    (policy (get-policy farmer policy-id))
    (farmer-info (get-farmer-info farmer))
  )
    (asserts! (is-some policy) err-no-policy)
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