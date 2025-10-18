Demonstration Plan
| File                                        | Purpose                   | Description                                                                    |
| ------------------------------------------- | ------------------------- | ------------------------------------------------------------------------------ |
| `testingdataset_with_actual_output.csv`     | User-uploaded test input  | Simulated daily actual output (per Pack_ID).                                   |
| `pv_fault_reliability_table_days.csv`       | Weibull reliability table | Stores parameters for each fault type (`shape_k`, `scale_lambda_days`, etc.).  |(optional for fault prediction)
| `solar_panel_installations_700x_seq100.csv` | Installation data         | Maps each `pack_id` to its installation date (for elapsed-day fault analysis). |(optional for fault prediction)




Demo Flow
Step 1: Upload & Configure
  Open the Solar Panel Quality Control dashboard (shiny::runApp()).
  Choose:
  Weather: “Sunny” 
  System Date: 2025-10-18
  Cpk Target: 1.33
  Baseline: “This batch”
  Upload test file testdataset1.csv.(you might need to open and save it to your laptop if it cannot be uploaded)(must include pack_id, actual_output).
  (Optional) Enable Fault Model and upload reliability datasets.
Step 2: Run the Analysis
  Click Compute ratio (append to history)
  → The app calculates ratio = actual_output / expected_performance.
  Threshold auto-calculates via LCL=max(0,μ−3σ×Cpk​)
  Low-performing packs trigger alerts (ratio < LCL).
Step 3: Visualize Results
  Observe KPI cards:
  Average Ratio, Alerts, Expected vs Actual totals, Threshold (Cpk).
  Adjust Severity filters to focus on underperforming packs.
  View Low Ratio Ranking (Pack_ID) to find the worst performers.
Step 4: Fault Prediction
  Click any › arrow next to an alert row →
  Opens modal showing possible fault causes:
  Uses Weibull model (from reliability + installation data)
  Or falls back to rule-based engine.
Step 5: Export
  Scroll down to Preview (current batch) →
  Review computed ratios and alerts.
  Click Download current batch CSV for archival.

Use Tips
  -Hover over the (i) icons next to each feature or title — they explain what the metric means, how it’s calculated, or what inputs it requires.
  -Preview (Current Batch) shows a scrollable 300px table of computed ratios — useful for checking before downloading.
  -Fault Prediction automatically uses Weibull reliability datasets if available; otherwise, it falls back to rule-based reasoning.



Tool Documentation 

  Solar Panel Quality Control is a Shiny dashboard for real-time photovoltaic performance analysis. It integrates:
    SPC analytics (Cpk-based 3σ thresholding)
    Batch KPI aggregation
    Interactive visualization & filtering
    Weibull reliability-based fault prediction


How Parameters Affect Results
| Parameter              | Impact                                                                     |
| ---------------------- | -------------------------------------------------------------------------- |
| **Weather**            | Sets `expected_performance` baseline (kW).                                 |
| **Cpk Target**         | Controls alert threshold width — higher Cpk = stricter QC.                 |
| **Threshold baseline** | Chooses whether Cpk stats come from *this batch* or *all session history*. |
| **Use AFR prior**      | Applies fault occurrence weighting in Weibull fault scoring.               |
| **Severity filters**   | Control what subset populates histograms and ranking lists.                |

Visual outputs:

KPI cards
Severity filter controls
Low Ratio Ranking (clickable modals)
Ratio distribution histogram
Scrollable preview table (300px)
CSV download

Codebook & Dataset README
Dataset: testdataset1.csv   testdataset2.csv
  Purpose: test input for ratio and alert computation
  Rows: 700 packs (each pack = 100 panels)
  Column pack_id	panel_ids	installation_end_date	time	actual_output			


Dataset: pv_fault_reliability_table_days.csv
  Purpose: Weibull reliability reference for PV fault modeling.
  Each row = fault mode with its Weibull life parameters.
| Column              | Type    | Example             | Meaning                               |
| ------------------- | ------- | ------------------- | ------------------------------------- |
| `fault`             | string  | `Inverter overheat` | Fault type                            |
| `shape_k`           | numeric | `2.5`               | Weibull shape parameter               |
| `scale_lambda_days` | numeric | `1500`              | Scale parameter (days)                |
| `MTTF_days`         | numeric | `1300`              | Mean Time To Failure                  |
| `Q2_5pct_days`      | numeric | `900`               | 2.5th percentile of expected lifetime |
| `Q97_5pct_days`     | numeric | `1800`              | 97.5th percentile lifetime            |
| `AFR_365days_pct`   | numeric | `0.3`               | Annualized failure rate (%)           |


Dataset: solar_panel_installations_700x_seq100.csv
Purpose: track installation completion dates for elapsed time computation.
| Column                  | Type   | Example           | Description                    |
| ----------------------- | ------ | ----------------- | ------------------------------ |
| `pack_id`               | string | `P0001`           | Matches pack in actual dataset |
| `installation_end_date` | date   | `2022-03-15`      | Installation completion date   |
| `panel_ids`             | string | `"P0001A–P0001J"` | Optional panel list string     |



  

Core Computations
Expected Output (kW):
| Weather | Expected per pack |
| ------- | ----------------- |
| Sunny   | 40,000            |
| Cloudy  | 35,000            |
| Rainy   | 32,000            |

Ratio Formula:

  Ratio=Actual Output/Expected Performance

Alert Threshold (Cpk 3-Sigma Control Limit):
	​
  LCL=max(0,μ−3σ×Cpk​)

Weibull Fault Model:







