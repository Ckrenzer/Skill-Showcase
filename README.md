# Skill-Showcase
Provides code samples for different coding applications and frameworks. My aim is to give you a big picture idea of 'what I know.'



# Languages

## SQL


## [R](https://github.com/Ckrenzer/Skill-Showcase/tree/main/R)
January 27th, 2020 was the day when I wrote my first ever line of code. It was written in R. Much has changed since the beginning of 2020, but I've since fallen in love with the language and decided to see how far I could take it! R is essentially my means of making a living and for that I am grateful. I'm a dplyr guy at heart, but data.table has been the primary tool my co-workers use. That necessarily makes data.table my primary tool in professional settings. Personally, I find data.table unintuitive and hard to fit on one line; data.table sacrifices intuition and readability for speed. You see, R was never meant to be 'fast'--it is supposed to simplify data analysis. To this end, dplyr complements R's strengths as a language quite well. If you want to know what I *really* think, data.table compensates R's slower run time speed but stifles the features of the language that make it stand out from competitors (Python, primarily) with its nebulous syntax. I think that if you have to rely on data.table for your everyday work, you are using the wrong tool for the job--Julia seems better suited for these tasks. I do appreciate data.table's backward compatibility, however.



## BASH
I was, unfortunately, born a Windows user. I used XP as a kid and never really knew other operating systems existed--except for those rich people with Apple laptops. Git for Windows and WSL2 changed everything. I use basic scripting in my day-to-day work and find it very handy for navigating through files. Nano is my editor of choice--I have no qualms with vim, but any time we spend learning one tool is time that could have been used learning another! I mostly edit in Bash for quick-and-dirty tasks and open up an IDE if the situation calls for more substantial work.






# [Data](https://github.com/Ckrenzer/Skill-Showcase/tree/main/data)

## [CSV](https://github.com/Ckrenzer/Skill-Showcase/tree/main/data/csv)

### LJMR
Short for 'La Junta Market Reports'. Collected from [Winter Livestock's weekly market reports](http://www.winterlivestock.com/lajunta.php#marketreport). See the full data set on [Winter-Livestock-Data](https://github.com/Ckrenzer/Winter-Livestock-Data).

Contains a subset of cattle sales I've been scraping since November, 2020. The column 'Reprod' corresponds to whether the livestock is a cow, bull, steer, or heifer. I didn't know a good name for this categorization, so 'Reprod' is what I tentatively decided on in December 2020. I couldn't find an industry standard. Unless there is a word used as the industry standard to classify this distinction, 'Reprod' is what I plan to continue using for the foreseeable future.

- **Date:** The date in which the auction ('Sale Tuesday') takes place. The first day is used for multi-day auctions.
- **Buyer:** The name of the buyer who made a purchase.
- **Quantity:** The number sold at auction.
- **Type:** The type of cattle. 'ang' corresponds to 'angus', 'lim' corresponds to 'limousine', and so on. 'clr' is a catch-all type for when there were too few observations worth caring about or only the color of the cattle was specified in the market report.
- **Weight:** The average weight of the cattle purchased.
- **Price:** The average price per pound, measured in cents.
- **Reprod:** The 'reproductive status' of the livestock. Was it a cow, bull, steer, or heifer?

### CHERAW_1_N_WEATHER_STATION
'CHERAW_1_N' is a weather station near La Junta, CO. This was scraped from Colorado State's [Colorado Climate Center](http://climate.colostate.edu/data_access.html). I took some liberties 'dirtying' the date column behind the scenes to showcase different cleaning techniques, so the format provided by CSU's website will look different.

- **record_date:** The date that weather data was collected. Recorded daily.
- **maxtemp:** The day's maximum recorded temperature in Fahrenheit.
- **mintemp:** The day's minimum recorded temperature in Fahrenheit.
- **pcpn:** The amount of precipitation in inches.
- **snow:** The amount of snow in inches.



## [TXT](https://github.com/Ckrenzer/Skill-Showcase/tree/main/data/txt)

### CRISTO

Contains Chapter 8 of *The Count of Monte Cristo* by Alexandre Dumas. I scraped this from [Project Gutenberg](https://www.gutenberg.org/cache/epub/1184/pg1184.txt) and attempted to clean it, so discrepancies between the text you see on the website and the text in the .txt file here may exist.
