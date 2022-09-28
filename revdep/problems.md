# cubble

<details>

* Version: 0.1.1
* GitHub: https://github.com/huizezhang-sherry/cubble
* Source code: https://github.com/cran/cubble
* Date/Publication: 2022-06-02 12:30:06 UTC
* Number of recursive dependencies: 136

Run `cloud_details(, "cubble")` for more info

</details>

## Newly broken

*   checking re-building of vignette outputs ... WARNING
    ```
    Error(s) in re-building vignettes:
    --- re-building ‘aggregation.Rmd’ using rmarkdown
    `summarise()` has grouped output by 'id'. You can override using the `.groups`
    argument.
    `summarise()` has grouped output by 'cluster', 'id'. You can override using the
    `.groups` argument.
    `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'
    Adding missing grouping variables: `id`
    Quitting from lines 123-131 (aggregation.Rmd) 
    Error: processing vignette 'aggregation.Rmd' failed with diagnostics:
    ...
    The key variable is named differently in the two datasets. Coerce the key to id
    to bind them together.
    Only bind the common variables from both datasets.
    --- finished re-building ‘matching.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘aggregation.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

