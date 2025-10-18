# OC_19-Six_Sigma_hackthon
For this hackathon, we developed a quality control system tracking the performance of solar panels
for NYSERDA to better understand the lifespan remaining and degree of maintenance needed in utility-scaled solar farms.
**Hypothesis case:**
We assume a hypothesis solar farm whoose total power out of 50MW, and 700 W per solar pannel. For statistical purpose, the major objects of our quality control system is based on the solar pannel package(each consists 100 solar pannels)

**Your inputs:**
Weather: Users are given four presets: Sunny(What we have now), Rainy(WIP), Cloudy(WIP), and Snowy(WIP)

Actual_Output.csv/: The actual power output each pack of solar panels generates. This allows us to compare it with the expected output and diagnose which pack is underperforming, allowing us to dive in further and identify the potential failure type.
(For the Hackathon we prepared two Testing Datasets: testing_datasets1.csv, and testing_datasets2.csv)

file path is locate at the OC_19_hackthon-six_sigma_hackthon, import the 


**Outputs**
The ShinyApp takes in weather conditions, using our recursive model to find an expected output from each pack (each contains 100 solar panels). Once it detects certain packs are performing under certain metrics(e.g. less than 85% of the expected output), the app will automatically pops out the following: 
[lifespan]: The lifespan of the Pack based on current date and installation date. It will serve as a coefficient of the Actual Output to filter out potential output shortage caused by aging solar panels.
explnation of the output interface
[Pack Id]: The Id tag of these underperforming packs for repairmen to locate their locations Installation time: An important metric used to predict what may the failure be. 
[output gap]: How much is has varied from the expected electric output
[severity]: dependent on the installation date and output gap, this metric assesses the emergency of the failure and whether the problem is significant or not.
[potential failure types]: From pv_fault_reliability_table_days.csv We have listed 10 common failures 

**feature of dashboard**:
Our dashboard also can export a csv file containg all the above collumns.

**process map**
![_cgi-bin_mmwebwx-bin_webwxgetmsgimg__ MsgID=1804448271866233056 skey=@crypt_ceb2b87c_d53b82a0a5df8b900ff8ebaf15c35c52 mmweb_appid=wx_webfilehelper](https://github.com/user-attachments/assets/50cfbfae-636c-46fe-bc1a-2a681e0e1955)

