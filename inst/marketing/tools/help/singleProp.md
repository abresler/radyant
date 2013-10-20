The single proportion (or one-sample) test is used to compare a sample proportion in our data to a hypothesized proportion in the population from which our sample data are drawn. This is important since we seldom have access to data for an entire population. The hypothesized value in the population is specified in the 'Comparsion value' box. 

We can perform either a one-tailed test (i.e., less than or greater than) or two-tailed test (see 'Alternative hypothesis'). In marketing we often use one-tailed tests because we want to evaluate if the available data provide evidence that a proportion is larger (or smaller) than some base-value (i.e., the value specified in the null-hypothesis).

#### Example

We have access to data from a random sample of grocery stores in the UK. Management will consider entering this geographical market if consumer demand for the product category exceeds 100M units, or, approximately, 1750 units per store. The average demand per store in the sample is equal to 1953. While this number is larger than 1750 we need to determine if the difference could be attributed to sampling error. 

You can find the information on unit sales in each store in the store sample in the __demand_uk.rda__ dataset. The dataset contains one variable, 'demand_uk'. Our null-hypothesis is that the average demand for a store is equal to 1750 unit. This is the number we enter into the 'Comparison value' box. Because we want to determine if the available data provides sufficient support to reject the null-hypothesis in favor of the alternative that average store demand in the UK is larger than 1750 we choose the 'Greater than' option for the 'Alternative hypothesis' drop-down.

![Single propotion - summary](figures/SingleMeanSummary.png)

![Single propotion - plot](figures/SingleMeanPlot.png)