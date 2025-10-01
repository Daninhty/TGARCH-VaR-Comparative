# TGARCH-Based Comparative Analysis of COMEX Gold Futures and the Indonesian Stock Exchange Composite Index with Granger Causality during Crisis Periods

## 📌 Overview
This project analyzes volatility spillovers between **COMEX Gold Futures** and the **Indonesian Stock Exchange Composite Index (IHSG)** during crisis periods.  
Using a **TGARCH model**, it estimates **Value at Risk (VaR)** for both markets and applies **Granger causality tests** to evaluate the direction of risk transmission and the safe-haven role of gold.

---

## 📂 Project Structure
1. **Data Collection & Preprocessing**  
   - Historical price data for COMEX Gold Futures and IHSG.  
   - Calculation of log returns during selected crisis periods.  

2. **Modeling with TGARCH**  
   - Estimation of volatility using TGARCH to capture asymmetry (leverage effects).  
   - Derivation of **VaR series** from conditional variance.  

3. **Comparative Analysis**  
   - Comparison of VaR between COMEX Gold Futures and IHSG.  
   - Examination of risk dynamics during financial and socio-economic crises.  

4. **Granger Causality Testing**  
   - Tests whether risk spillovers flow from gold to stocks or vice versa.  
   - Identifies the hedging and safe-haven properties of gold.  

5. **Reporting & Visualization**  
   - Tables summarizing TGARCH parameters, VaR values, and causality results.  
   - Graphs of return series, volatility clustering, and VaR dynamics.  

---

## 🚀 Technologies & Frameworks
- **R Programming**  
  - `quantmod` (financial data retrieval)
  - `ggplot2` (visualization)
  - `dplyr` (data manipulation)
  - `tseries`, `FinTS`, `lmtest` (time series tests)
  - `fBasics` (descriptive statistics)
  - `rugarch`, `fGarch`, `rmgarch` (GARCH/TGARCH volatility modeling)
  - `GAS` (generalized autoregressive score models)
  - `tstests` (additional time series testing) 

---

## 📊 Expected Outcomes
- TGARCH-based VaR estimates for both COMEX and IHSG during crises.  
- Evidence of volatility asymmetry and clustering in both markets.  
- Comparative insights on risk levels between gold and equities.  
- Granger causality results showing whether COMEX gold acts as a hedge or safe haven for the Indonesian stock market.  
