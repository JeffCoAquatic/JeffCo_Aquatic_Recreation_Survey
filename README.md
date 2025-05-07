# Jefferson County Aquatic Recreation Survey
This repository contains the files and code used to clean (QA/QC), process, and summarize data from the 2025 *Jefferson County Aquatic Recreation Survey*. This survey was implemented to gague public interest in a new aquatic recreation facility in located in the Port Hadlock (mid-county) location. The survey ran from 2/1/2025 - 3/31/2025 and all survey responses recieved during this period are included in the working dataset. 

**Analysis and code contained within this repository are preliminary**. 
An initial preliminary analysis was conducted to prepare the data for the Board of County Commissioners meeting on April 7th. The final analysis requires additional cleaning (QA/QC) of the data refinement of the summaries, which is currently underway. Code will be updated as the analysis progresses towards completion.  

Data and files contained in this repository include the following:
+ raw dataset with all responses (some fields removed for privacy; *Data/JAC_Survey_3.31.25.csv*)
+ file with submitted comments and catagorizations used for the sentiment analysis (*CommentScoringSheet_share.xlsx*)
+ QA/QC code that filters data for resident responses, corrects formatting issues with, and recategorizes the 'other' entries as appropriate (*DataQAQC.R*)
+ Master R Markdown script that summarizes and produces outputs for each survey question (*JAC Survey Results.Rmd*)
+ Power Point Presentation of the preliminary analysis presented to the BOCC on April, 7th 2025 (*BOCC Survey Presentation_April 7.pdf*)
  
  
