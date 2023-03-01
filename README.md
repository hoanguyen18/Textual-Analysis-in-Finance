# Textual-Analysis
🤖 Measure political partisanship score using speech of U.S. congress man/women and apply it to speech by the Board of Governors of the U.S. FED

🤖 As we know, In contrast to the U.S. Congress, the Federal Reserve System should be a non-partisan institution. 

🤖 However, as individual members of the Federal Open Market Committee [FOMC] are appointed by politicians, they might have an incentive to engage in political partisan speech. 

-> We want to know how much partisanship among financial regulators!

🤖 Approach: to use speeches from members of the U.S Congress from 2001 to 2017 to construct a textual partisanship score for each phrase 

Apply the measurement to speeches by members of the FOMC to calculate how much partisanship is present within the FOMC.

🤖 Data is scraped from Congress speeches, FOMC speeches. We use regular expression to remove unnessary words and characters, consolidate to root form of each word

Next, we break the speeches into smaller phrase which is bigrams and we calculate the probability that phrase is mentioned by whether republicans or democrats. 
Then we clean the FOMC speeches in the same way and calculate the partisanship score equals the probability of R/D from the last step (that it appears in Congress) * with the frequency that phrase is spoken by Democrat/Repulican-affiliated FOMC-members. 

🤖 With that measure we want to capture the extent that members of the FOMC sound like congressional politicians in their own party. 

🤖 If no partisanship were present in FOMC meetings, we would assume a partisanship score of 0.5. 
