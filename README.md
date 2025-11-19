# reference-checker

A collection of functions to check for the existence of references (in Web of Science) 
in a reference list.

## How to use:

1. Get an API key from the Clarivate Developer Portal and subscribe to the [Web of
   Science Starter API](https://developer.clarivate.com/apis/wos-starter). 
   Authentication is by API key (X-ApiKey).

2. Store the key in ~/.Renviron (recommended) as `WOS_API_KEY=XXXX`.

3. Prepare your references`.txt` file, with one reference per line.

4. Run `source("verify_wos_existence.R")` or from the terminal:
   ```
   Rscript verify_wos_existence.R refs.txt
   ```
   or, if providing a custom output file name,
   ```
   Rscript verify_wos_existence.R refs.txt results.csv
   ```
   
## Notes and limitations

* API coverage and plans: The Starter API provides basic metadata and supports 
  simple field‑tag searches (e.g., DO, TI, AU, PY) with per‑plan request limits; 

* Web of Science API Expanded offers richer metadata and full field‑tag support 
  if your institution licenses it. For simple “existence” checks, Starter is sufficient.
  
* Rate limits: The script includes a modest inter‑request delay (0.25 sec); 
  try to increase it if you hit HTTP 429 responses (i.e. too many requests). 
  Plan limits vary (e.g., free plan ~50 requests/day; institutional plans allow more).
  
* Query construction: This script assumes each line is a full citation. Parsing 
  The Starter API’s supported field tags are listed in its Swagger (e.g., DO, TI, AU, PY).
  
* Alternative in R: If you already use the legacy wosr package (Web Services / InCites clients),
  note it authenticates via session IDs (username/password) and targets different endpoints; 
  it is not the same as the Starter API’s API‑key model. For new workflows, Clarivate 
  recommends the newer Starter/Expanded REST APIs.


Scripts and notes above were built with the help of MS365 Copilot, powered by ChatGPT 5.0.
