import quandl
import os
import datetime
import Commodity

quandlToken = "8MsMk6Rkrm3dz3U5Fr4P"
quandl.ApiConfig.api_key = quandlToken

cl = Commodity.Commodity('CL', os.path.expanduser('~/Data'))
cl.update(datetime.date(1995, 3, 1), datetime.date(1996, 2, 1))
