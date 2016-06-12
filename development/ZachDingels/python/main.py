import quandl
import os
import datetime

quandlToken = "8MsMk6Rkrm3dz3U5Fr4P"
quandl.ApiConfig.api_key = quandlToken

cl = Commodity('CL', os.path.expanduser('~/Data'))
cl.update(datetime.date(1995, 1, 1), datetime(1996, 1, 1))
