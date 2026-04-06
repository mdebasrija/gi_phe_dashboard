# Frequently Asked Questions (FAQ)





### **What is the source of the data?**
This data comes from the phenotype **(blood biochemistry, socio-demographic and anthropometric information)** data collected by the 13 participating centres as part of the GenomeIndia Project. The **GenomeIndia Project** is a pan-India initiative funded by the Department of Biotechnology (DBT), Government of India, to generate whole genome sequence data from diverse Indian populations. This dataset was last updated in **November 2025**. This dashboard comes as a companion to the dedicated GenomeIndia manuscript for phenotypic patterns (2026).

### **Why are there more samples than the GenomeIndia manuscript?**
Although 9,768 samples were sequenced, a larger number of samples were collected and phenotyped. We have built the dashboard using data from all available 17,777 samples with usable phenotype data. This includes 9,330 samples out of the 9,768 sequenced samples that we had phenotype data for, and 8,447 more samples. 

### **What do the ethnicity names mean?**
DBT policy restricts the release of ethnicity names. We have used a systematic coding for each ethnicity that captures their **biogeographic region**, **linguistic family**, and **tribe/non-tribe status**.

The mapping follows this structure:
**LinguisticGroup_Biogeography_TribeStatus_PopulationNumber**
**XX_YYY_N_MM**

**Interpretation:**

- **XX – Linguistic group**  
  Two-letter code for the linguistic origin of the population:  
  - `AA` = Austro-Asiatic  
  - `DR` = Dravidian  
  - `IE` = Indo-European  
  - `TB` = Tibeto-Burman  

- **YYY – Biogeographical region**  
  Three-letter code for the biogeographic region where the population is primarily found.  

  - `BPV` = Brahmaputra Valley
  - `CHR` = Central Himalayas
  - `ECP` = Eastern Coastal Plains
  - `EGH` = Eastern Ghats
  - `EPL` = Eastern Plateau
  - `ERP` = Eastern Riverine Plains
  - `NGH` = Nilgiri Hills
  - `NCH` = North Central Highlands
  - `NDN` = North Deccan
  - `NER` = North Eastern Range
  - `NRP` = Northern Riverine Plains
  - `SCH` = South Central Highlands
  - `SDN` = South Deccan
  - `WCP` = Western Coastal Plains
  - `WHR` = Western Himalayas
  - `WPL` = Western Plains


- **N – Tribe/Non-Tribe status**  
  - `1` = Tribal  
  - `2` = Non-tribal  

- **MM – Population number**  
  Two-digit number to distinguish multiple populations with the same linguistic group, biogeographic region, and tribe/non-tribe status.  

**Special case:**  
A continentally admixtured outgroup of predominantly African origin is coded as **CAO**.

### **What can this dashboard be used for?**
This dashboard can be used to explore trait variations and assess data coverage across linguistic and ethnicity groups, age and gender structures, as well as state and collection centres, using the data collected as part of the GenomeIndia Project.

### **Which traits are covered in this dashboard?**
We have covered variation in 38 clinical and anthropometric traits including lipids, liver enzymes, glucose, blood cell attributes, blood pressure and body size measures. 

### **What preprocessing has been performed on the data?**
We have removed outlier values from the raw data in two passes. First, we removed only the values which looked grossly out of range (actual values specified in the accompanying manuscript). 

In the second pass, we have:
- Removed all values which are greater than 3 standard deviations away from the mean for both tails, for a distribution where the skew is between -1 and 1
- Removed all values which are greater than 3 standard deviations away from the mean from the higher skewed tail for the remaining variables.

There are multiple possible approaches to outlier removal that would be the most logical in terms of the question being asked. For users who think a different method for outlier removal may be more suited to their uses, we request you to formally apply for data access.



### **How can I get access to the data?**
No data can be downloaded off this dashboard. This platform is a tool for visualization alone. For users who require direct access to the data, we request you to apply for data access to the consortium.

### **I noticed some interesting patterns in the data. Can I publish them?**
Please cite the GenomeIndia flagship manuscript and the phenotype data manuscript while using any of these data.

<!---
### **The geographical distribution tab is showing the error message "An error has occurred. Check your logs or contact the app author for clarification". How can I fix that?**
This error is often temporary. Please allow a couple of minutes for the map to load after clicking 'Update Map', and in most cases, the error resolves itself. If that does not fix the issue, please reload the dashboard. As generating these maps is a memory-intensive process, it takes some time to load. Thank you for your patience.
-->

### **Why do I see the message 'Selection too granular to display safely (n < 5). Please broaden filters.' for certain combinations of parameters?**
Our dashboard is designed to protect the privacy of participants. If you apply filters that narrow down the data too much (for example, a very specific age group within a single community), the result might only include one or two people. Showing that information could risk revealing individual data. To prevent this, the dashboard only shows results when there are at least 5 people in a group according to the selection. If your selection is too narrow, you’ll see a message instead of a chart or table. To view results, please try broadening your filters.

### **Where was this dashboard developed, and who do I contact if I have dashboard-related queries beyond what is mentioned in the FAQ?**
This dashboard was developed at the Centre for Brain Research, which is the co-ordinating centre for the GenomeIndia Project, and we sincerely acknowledge the hosting and allied technical support provided by the Indian Biological Data Centre. Please contact Dr. Shweta Ramdas (shwetaramdas@cbr-iisc.ac.in) or Dr. Sanjay Deshpande (sanjay.deshpande@ibdc.rcb.res.in) in case of further queries.
