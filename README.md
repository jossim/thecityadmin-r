# The City Admin API in R

An interface to [The City admin API](https://api.onthecity.org/docs/admin) for the R programming language.


## Examples

Create an instance of TheCity object.
```R
library("TheCityAdmin")
city = TheCity(key = <secret key>, token = <user token>)
```

Fetch 1 page of checkin data, with a parameter.
```R
fetch.checkins(city, total = 1, params = c(include_checked_out = 'true'))
```
