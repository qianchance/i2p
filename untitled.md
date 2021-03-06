# Data Biography

## Student Number: 20202007

---

### 1. Who collected the data?

The data was provided by Murray Cox, who compiled and analyzed public and available information on Airbnb in cities around the world.

---

### 2. Why did they collect it?

It allows individuals who are interested to explore how Airbnb is being used in cities around the world and how it competes with the residential housing market.[5] It also guides comparing the business snapshots provided by Airbnb with the captured data by Inside Airbnb to ensure the authenticity and transparency of the information

---

### 3. How was it collected?

The data does not use ‘private’ information and is gleaned from public and available information on Airbnb site. It is a snapshot of the listing available at a particular time. Then, data is verified, cleaned up, analyzed, and aggregated. Besides, Inside Airbnb uses the occupancy model called San Francisco Model to quantify Airbnb’s impact on housing and estimate the rental frequency and income from listing. This model firstly takes a review rate of 50% as estimated booking. Then, the occupancy rate is calculated by multiplying the average length of stay in each city by the estimated bookings.

---

### 4. What useful information does it contain?


This data set mainly contains information related to housing: id、name、description、price、amenities、latitude、longitude、type of room; 
information about hosts: their id、location、verification、the total listing of hosts 
information about dates: minimum and maximum nights, available date in the next 365 days, or whether reservations are available instantly.
Information about reviews: characters of popular housing and hosts.

---

### 5. What useful information is it missing?

It is missing information about the bedroom and license. Some unwelcome listings may not have been made public. Information about hosts may not include the language they used, resulting in language barriers between the hosts and tenants. Hosts’ information is hidden because they may have criminal records


---

### 6. To what extent is the data 'complete'?

The data set contains 74 types of information and 74,188 houses. 10,000 houses and 42 types of information were selected as samples for analysis. It was found that the id of one house was not a number, which made the data of this house unavailable. Besides, the information about the bedroom and license were also completely useless because all the information was null in samples. For the data about hosts' response and acceptance rate, almost half of the sample was null. And there were 2489 blank values in the column of hosts_about, which may be because the hosts were reluctant to disclose. Obviously, these five types of information were not complete. The data set contained some randomly missing, for example, nearly 2000 pieces of reviews about the location, communication, and cleanliness of houses were empty. Also, more than 7 kinds of information in the 1,812 houses had null values, indicating that these listings were incomplete. Therefore, only almost 8188 houses had relatively complete information in the sample.

---

### 7. What kinds of analysis would this support?

Price can be analyzed, researchers can calculate range, mean, standard deviation, IQR-standardised score, Z-score of price, and so on. And then plot histogram, KDE plots, Boxplot, and Scatterplots. In addition, Pearson’s coefficient is used to measure the correlation between price and other variables such as location or related reviews. It can also reveal the distribution of prices across different areas. In terms of housing analysis, it allows visualizing the data by combining longitude and latitude, which provides a straightforward map of the listings showing prices on it. It can find out which area the houses are mainly distributed in and which category of property or room in the same areas are the most popular. According to the analysis of hosts, the average listings owned by each host is revealed. It can find that which hosts are the most popular, and the average time and efficiency of hosts reply are calculated.

---

### 8. What kinds of analysis would it _not_ support?

In terms of the data itself, it exists structurally missing, there are nearly 1,200 houses in the sample with incomplete information in different columns. Moreover, empty values exist in the bedroom and license columns. And the columns of host_response_rate and host_about cannot be analyzed because there are too many missing values. 
For data ethics, analysis of some personal information about hosts would not be supported. Firstly, the privacy of the landlord may need to be protected. Secondly, it may cause gender, age, and racial discrimination. Because tenant or hosts may take race, age, or gender into account when choosing a property or deciding whom to rent to, thereby causing prejudge or discrimination

---

### 9. Which of the uses presented in Q.7 and Q.8 are _ethical_?

In data ethics, issues of data use, ownership, dependability of researchers, and privacy are all concerns. [3] Obviously, the analysis of price and houses should be ethical. The main reason for this data disclosure is hosts tend to gain revenue from renting out their properties or rooms. For the hosts, there is a balance between risk and return or the return outweighs the risk. It also helps tenants to find favorite types of listings within a reasonable price range.
The use of host information is the most controversial. General Data Protection Regulations (GDPR) emphasize that researchers need to protect the basic rights of natural persons in the collection and processing of data related to personal data, and natural persons can control their personal data. [4] Cause the contents of the dataset involves the privacy of hosts, the hosts need to be informed and consent to the data storage. [1] As a result, if hosts do not consent to the data being used for analysis, that is a violation of privacy and unethical behavior. According to the statement from Inside Airbnb, no private information was used in the dataset, and all the data was displayed on Airbnb. Null values in hosts’ information may not have been approved by the hosts. So, the available data about hosts should be ethical. However, the hosts’ information may relate to gender, race, and sexual orientation. This raises potential discrimination. [2] If the host is a homosexual, the tenant may reject the house due to prejudice.

### References

[1] Bemt, V. van den, J. Doornbos, L. Meijering, M. Plegt, and N. Theunissen. 2018. “Teaching Ethics When Working with Geocoded Data: A Novel Experiential Learning Approach.” *Journal of Geography in Higher Education* 42 (2): 293–310. https://doi.org/https://doi.org/10.1080/03098265.2018.1436534.

[2] Cheng, M., and C. Foley. 2018. “The Sharing Economy and Digital Discrimination: The Case of Airbnb.” *International Journal of Hospitality Management* 70: 95–98. https://doi.org/10.1016/j.ijhm.2017.11.002.

[3] Crawford, K., and M. Finn. 2015. “The Limits of Crisis Data: Analytical and Ethical Challenges of Using Social and Mobile Data to Understand Disasters.” *GeoJournal* 80 (4): 491–502. https://doi.org/https://doi.org/10.1007/s10708-014-9597-z.

[4] GDPR. Available online at https://gdpr-info.eu/art-4-gdpr/ (last accessed Nov 23, 2020). Google Scholar

[5] Inside Airbnb. *About Inside Airbnb*. Available at: http://insideairbnb.com/about.html.




