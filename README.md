# OC_19-Six_Sigma_hackthon
#  Scope, User, and Design Rationale
For this hackathon, we developed a quality control system tracking the performance of solar panels
for NYSERDA to better understand the lifespan remaining and degree of maintenance needed in utility-scaled solar farms.

**Hypothesis case:**
We assume a hypothesis solar farm whoose total power out of 50MW, and 700 W per solar pannel. For statistical purpose, the major objects of our quality control system is based on the solar pannel package(each consists 100 solar pannels)

Excellent — this part of your Hackathon submission is essentially the **“Project Scope & Design Justification”** section in your README, explaining **why** your tool fits the prompt, who it’s for, and how it was designed to be practical and user-friendly.

Below is a polished, hackathon-ready write-up that directly hits each of the 3 grading criteria — 15 pts total — formatted so you can paste it into your final README under a new section titled:

---

This project integrates **Statistical Process Control (SPC)** principles (Cpk-based 3-sigma thresholds) and **Weibull reliability analysis**, both widely used in industrial engineering quality frameworks.
By combining these methods, the tool enables users to:

* Continuously evaluate production ratios (`actual_output / expected_performance`);
* Identify performance anomalies automatically;
* Diagnose the most likely fault causes based on life-cycle reliability models.

---

### **Intended User and Use Case **

The primary user is a NYSERDA or solar plant operations engineer responsible for overseeing field performance across hundreds of solar panel packs (each pack = 100 panels).

**Use case example:**
An engineer uploads the latest daily “actual output” data, selects weather conditions, and immediately sees:

* Which packs fall below the expected ratio threshold,
* The severity distribution of performance deviations, and
* Potential root causes predicted by fault models.

The dashboard’s interface, tooltips `(i)` icons, and auto-computed KPIs are tailored for **non-programmer technical staff** — users who understand system metrics but may not know R or statistical formulas.
Thus, the tool directly supports decision-making for field reliability, minimizing the need for specialized data-science expertise.

---

### **Input Efficiency & User Simplicity **

The tool is intentionally designed with minimal, practical user inputs, avoiding unrealistic data requirements:

| Input                             | Description                                                     | Why it’s user-friendly                                                               |
| --------------------------------- | --------------------------------------------------------------- | --------------------------------------------------------------------- |
| Weather selection                 | Choose from `Sunny`, `Cloudy`, or `Rainy`.                      | Simple categorical input; no need to provide full meteorological data |
| Actual Output CSV                 | One file containing `pack_id` and `actual_output`.              | A single standard export from plant monitoring software; requires no pre-processing. |
| *(Optional)* Fault model data     | Pre-trained Weibull reliability table and installation records. | Provided as default within the tool; user may override if they have updated data.    |

All other calculations — expected performance, ratio, SPC threshold, and fault prediction — are performed automatically in-app.
The interface includes **(i)** tooltips next to every field and metric, explaining each step and expected format.

This ensures the tool is practical for real operators, requiring only realistic, readily available information while maintaining analytical rigor.


**Statistical Method logic**
We're training an XGBoost model to predict a continuous target y from features X. XGBoost builds many shallow decision trees sequentially; each new tree corrects the residual errors of the previous ones. The final prediction is the weighted sum of all trees.
<img width="843" height="830" alt="_cgi-bin_mmwebwx-bin_webwxgetmsgimg__ MsgID=1232959916351484610 skey=@crypt_ceb2b87c_d53b82a0a5df8b900ff8ebaf15c35c52 mmweb_appid=wx_webfilehelper" src="https://github.com/user-attachments/assets/93ec8829-dbe2-460d-9d71-1a8cbe22c74e" />

**Your inputs:**
Weather: Users are given three presets: Sunny(What we have now), Rainy(WIP), and Cloudy(WIP).

Actual_Output.csv/: The actual power output each pack of solar panels generates.The user should easily get this information from IoT. This allows us to compare it with the expected output and diagnose which pack is underperforming, allowing us to dive in further and identify the potential failure type.
(For the Hackathon we prepared two Testing Datasets: testing_datasets1.csv, and testing_datasets2.csv)



**#Outputs**
The ShinyApp takes in weather conditions, using our recursive model to find an expected output from each pack (each contains 100 solar panels). Once it detects certain packs are performing under certain metrics(e.g. less than 85% of the expected output), the app will automatically pops out the following: 

**1) KPI Cards (top right)**
Average ratio — mean of all pack ratios in the current batch (unitless).
Panels in alert — number of packs whose ratio is below the current threshold.
Expected (total, kW) — sum of expected performance across all packs (based on chosen weather).
Actual (total, kW) — sum of uploaded actual outputs.
Current threshold — the alert threshold shown as a percentage.
Note: tooltips explain whether this comes from Cpk LCL or the fixed 85% fallback.

**2) Severity Filters (affects charts & ranking)**
Bucket filter — quick ranges: <0.70, 0.70–0.85, 0.85–1.00, >1.00, or All.
Continuous range — free slider to focus on any ratio interval (default 0–1.5).
Only alerts — show only packs below the current threshold.

**3) Low Ratio Ranking (Pack_ID)**
Displays up to Top 20 (filtered) packs in ascending or descending order:
Worst → Top or Top → Worst.
Each row shows: Pack_ID, Ratio, Actual, Expected, and Alert/OK.
Clickable arrow (›) opens a Possible Fault Prediction modal for that pack.

**4) Fault Prediction Modal (per Pack_ID)**
Header summary: Weather, Ratio, Actual (kW), Expected (kW), Threshold.
If Weibull datasets are available:
Top-3 likely faults with confidence (as % of relative probability) and an action note.
Timing context: Installation date, Detection date, Elapsed days.
If datasets are missing:
Falls back to a rule-based list (e.g., soiling, inverter outage, etc., with suggested actions).

**5) Ratio Distribution (Histogram)**
Histogram of filtered ratios with a vertical line at the current threshold.
Useful to visualize how many packs cluster below alert cutoffs.

**6) Preview (current batch)**
Scrollable 300px panel showing first rows of the computed result table for this run.
Download current batch CSV button exports the full computed table.

**feature of dashboard**:
Our dashboard also can export a csv file containg all the above collumns.

**process map for our control system：**
![_cgi-bin_mmwebwx-bin_webwxgetmsgimg__ MsgID=1804448271866233056 skey=@crypt_ceb2b87c_d53b82a0a5df8b900ff8ebaf15c35c52 mmweb_appid=wx_webfilehelper](https://github.com/user-attachments/assets/50cfbfae-636c-46fe-bc1a-2a681e0e1955)



