# Crop Insurance Smart Contract

A decentralized crop insurance system built on Stacks blockchain that automatically pays farmers based on weather data.

## Overview

This smart contract enables farmers to purchase insurance policies for their crops. Payouts are triggered automatically when adverse weather conditions are reported by a trusted oracle.

## Features

- Farmer registration
- Insurance policy purchase
- Weather data submission by oracle
- Automatic claim calculation based on drought index
- Transparent payout system

## Contract Functions

### For Farmers

- `register-farmer`: Register as a farmer in the system
- `purchase-insurance`: Buy an insurance policy for a specific crop and region
- `claim-insurance`: Claim insurance payout when eligible
- `get-farmer-info`: View your registration and policy information

### For Oracle

- `submit-weather-data`: Submit weather data for a specific region

### For Contract Owner

- `add-region`: Add a new region to the system
- `add-crop`: Add a new crop type to the system
- `set-oracle-address`: Update the oracle address
- `set-min-premium`: Set the minimum premium amount

## How to Use

1. **Register as a farmer**
   ```
   (contract-call? .crop-insurance register-farmer)
   ```

2. **Purchase insurance**
   ```
   (contract-call? .crop-insurance purchase-insurance 
     u1                ;; region-id
     u2                ;; crop-id
     u1000000          ;; premium amount (in microSTX)
     u10000000         ;; coverage amount (in microSTX)
     u144              ;; duration in blocks (approximately 1 day)
   )
   ```

3. **Check your policy**
   ```
   (contract-call? .crop-insurance get-policy tx-sender u1)
   ```

4. **Set beneficiary delegate** (optional)
   ```
   (contract-call? .crop-insurance set-beneficiary 'SP2ABC...)
   ```

5. **Claim insurance** (when eligible)
   ```
   ;; Farmer claims their own policy
   (contract-call? .crop-insurance claim-insurance tx-sender u1)
   
   ;; Beneficiary claims on farmer's behalf
   (contract-call? .crop-insurance claim-insurance 'SP1FARMER... u1)
   ```

## Payout Calculation

Payouts are calculated based on the drought index:
- Drought index > 70: 100% of coverage
- Drought index > 50: 75% of coverage
- Drought index > 30: 50% of coverage
- Drought index ≤ 30: No payout

## Setup for Development

1. Install [Clarinet](https://github.com/hirosystems/clarinet)
2. Clone this repository
3. Run `clarinet console` to interact with the contract

## Testing

Use the Clarinet console to test the contract functionality:

```
clarinet console
```

Then you can execute contract calls to test the functionality.