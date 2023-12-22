# hetGP4cast

hetGP4cast is an R package that allows users to create climatological null forecasts. 

Climatological models are useful for forecasting ecological phenomena, as they can capture long-term trends and tendencies. Climatological models also provide important baseline forecasts for researchers developing and testing new ecological forecasting models; for example, climatological models can generate 'null model' forecasts that can be used to determine the relative improvement in skill of new models. Many ecological variables that are increasingly being forecasted are characterized by heteroscedasticity over time and other dimensions. However, climatological models to date have typically not accounted for input-dependent variability, limiting their use for ecological forecasting. The incorporation of a non-constant variance over at least one dimension would improve uncertainty quantification (UQ) of climatological forecast models and could dramatically improve the prediction accuracy and quality of UQ of baseline forecasts. Heteroscedastic Gaussian process models (hetGPs) are ideal for constructing climatological models; however, while hetGPs can be fitted with existing software packages (i.e. the `hetGP` package; Gramacy and Binois 2021), they are not as easily accessible to non-statisticians. In response to this need, we developed the `hetGP4cast` package to fit hetGPs to data targeted for environmental applications.

To download this package use: devtools::install_github("maikeh7/hetGP4cast")


View the vignette here: https://rpubs.com/dutchie/vignette

Raw vignette files are located here: https://zenodo.org/records/10064756

## References

```
@article{binois2021hetgp,
  title={hetgp: Heteroskedastic Gaussian process modeling and sequential design in R},
  author={Binois, Micka{\"e}l and Gramacy, Robert B},
  year={2021}
}
```
