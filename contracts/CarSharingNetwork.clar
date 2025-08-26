;; CarSharing Network Smart Contract
;; Peer-to-peer vehicle sharing with insurance integration and usage-based pricing

;; Define the fungible token for payments
(define-fungible-token carshare-token)

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-authorized (err u101))
(define-constant err-vehicle-not-available (err u102))
(define-constant err-insufficient-payment (err u103))
(define-constant err-invalid-amount (err u104))
(define-constant err-booking-not-found (err u105))
(define-constant err-vehicle-not-found (err u106))
(define-constant err-invalid-duration (err u107))

;; Data structures
(define-map vehicles
  { vehicle-id: uint }
  {
    owner: principal,
    make-model: (string-ascii 50),
    year: uint,
    price-per-hour: uint,
    insurance-coverage: uint,
    is-available: bool,
    location: (string-ascii 100)
  }
)

(define-map bookings
  { booking-id: uint }
  {
    vehicle-id: uint,
    renter: principal,
    start-time: uint,
    duration-hours: uint,
    total-cost: uint,
    insurance-fee: uint,
    status: (string-ascii 20)
  }
)

;; Data variables
(define-data-var next-vehicle-id uint u1)
(define-data-var next-booking-id uint u1)
(define-data-var platform-fee-rate uint u5) ;; 5% platform fee
(define-data-var insurance-rate uint u10) ;; 10% insurance fee

;; Function 1: Register Vehicle
;; Allows vehicle owners to register their vehicles for sharing
(define-public (register-vehicle 
    (make-model (string-ascii 50))
    (year uint)
    (price-per-hour uint)
    (insurance-coverage uint)
    (location (string-ascii 100)))
  (let
    (
      (vehicle-id (var-get next-vehicle-id))
    )
    (begin
      ;; Validate inputs
      (asserts! (> price-per-hour u0) err-invalid-amount)
      (asserts! (> year u1990) err-invalid-amount)
      (asserts! (> insurance-coverage u0) err-invalid-amount)
      
      ;; Register the vehicle
      (map-set vehicles
        { vehicle-id: vehicle-id }
        {
          owner: tx-sender,
          make-model: make-model,
          year: year,
          price-per-hour: price-per-hour,
          insurance-coverage: insurance-coverage,
          is-available: true,
          location: location
        }
      )
      
      ;; Increment vehicle ID for next registration
      (var-set next-vehicle-id (+ vehicle-id u1))
      
      ;; Print registration event
      (print {
        event: "vehicle-registered",
        vehicle-id: vehicle-id,
        owner: tx-sender,
        make-model: make-model,
        price-per-hour: price-per-hour
      })
      
      (ok vehicle-id)
    )
  )
)

;; Function 2: Book Vehicle
;; Allows users to book available vehicles with automatic insurance and pricing calculation
(define-public (book-vehicle 
    (vehicle-id uint) 
    (duration-hours uint))
  (let
    (
      (vehicle-data (unwrap! (map-get? vehicles { vehicle-id: vehicle-id }) err-vehicle-not-found))
      (booking-id (var-get next-booking-id))
      (base-cost (* (get price-per-hour vehicle-data) duration-hours))
      (insurance-fee (/ (* base-cost (var-get insurance-rate)) u100))
      (platform-fee (/ (* base-cost (var-get platform-fee-rate)) u100))
      (total-cost (+ base-cost insurance-fee platform-fee))
    )
    (begin
      ;; Validate booking request
      (asserts! (get is-available vehicle-data) err-vehicle-not-available)
      (asserts! (> duration-hours u0) err-invalid-duration)
      (asserts! (not (is-eq tx-sender (get owner vehicle-data))) err-not-authorized)
      
      ;; Check if renter has sufficient balance (assuming they have tokens)
      (asserts! (>= (ft-get-balance carshare-token tx-sender) total-cost) err-insufficient-payment)
      
      ;; Transfer payment to contract
      (try! (ft-transfer? carshare-token total-cost tx-sender (as-contract tx-sender)))
      
      ;; Mark vehicle as unavailable
      (map-set vehicles
        { vehicle-id: vehicle-id }
        (merge vehicle-data { is-available: false })
      )
      
      ;; Create booking record
      (map-set bookings
        { booking-id: booking-id }
        {
          vehicle-id: vehicle-id,
          renter: tx-sender,
          start-time: block-height, ;; Using block height as timestamp
          duration-hours: duration-hours,
          total-cost: total-cost,
          insurance-fee: insurance-fee,
          status: "active"
        }
      )
      
      ;; Increment booking ID
      (var-set next-booking-id (+ booking-id u1))
      
      ;; Print booking event
      (print {
        event: "vehicle-booked",
        booking-id: booking-id,
        vehicle-id: vehicle-id,
        renter: tx-sender,
        total-cost: total-cost,
        insurance-fee: insurance-fee,
        duration-hours: duration-hours
      })
      
      (ok {
        booking-id: booking-id,
        total-cost: total-cost,
        insurance-fee: insurance-fee,
        start-time: block-height
      })
    )
  )
)

;; Read-only functions for data retrieval

;; Get vehicle details
(define-read-only (get-vehicle (vehicle-id uint))
  (map-get? vehicles { vehicle-id: vehicle-id }))

;; Get booking details  
(define-read-only (get-booking (booking-id uint))
  (map-get? bookings { booking-id: booking-id }))

;; Get current platform fee rate
(define-read-only (get-platform-fee-rate)
  (ok (var-get platform-fee-rate)))

;; Get current insurance rate
(define-read-only (get-insurance-rate)
  (ok (var-get insurance-rate)))

;; Get next vehicle ID
(define-read-only (get-next-vehicle-id)
  (ok (var-get next-vehicle-id)))

;; Get next booking ID  
(define-read-only (get-next-booking-id)
  (ok (var-get next-booking-id)))

;; Administrative functions (owner only)

;; Update platform fee rate
(define-public (set-platform-fee-rate (new-rate uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (<= new-rate u20) err-invalid-amount) ;; Max 20% fee
    (var-set platform-fee-rate new-rate)
    (ok true)
  )
)

;; Update insurance rate
(define-public (set-insurance-rate (new-rate uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (<= new-rate u30) err-invalid-amount) ;; Max 30% insurance fee
    (var-set insurance-rate new-rate)
    (ok true)
  )
)