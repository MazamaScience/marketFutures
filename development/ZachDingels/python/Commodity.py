import pandas as pd
import os

CONTRACT_MONTH_CODES = ['F', 'G', 'H', 'J', 'K', 'M', 'N', 'Q', 'U', 'V', 'X', 'Z']

class Commodity(object):
    """docstring for Commodity"""
    def __init__(self, code, pathToDataDir):
        self.code = code
        self.pathToDataDir = pathToDataDir
        self.contracts = set()

        self.load()

    @property
    def pathToDataDir(self):
        return this.pathToDataDir
    @pathToDataDir.setter
    def pathToDataDir(self, path):
        if os.path.isdir(path):
            self.pathToDataDir = path
            self.pathToData = os.path.join(self.pathToDataDir, self.code + '.csv')
            if not os.path.exists(self.pathToData):
                pd.DataFrame.to_csv(self.pathToData)
        else:
            raise FileNotFoundError('Could not find a directory with the path: %s.' % path )

    def save(self):
        self.data.to_csv(self.pathToData)

    def load(self, pathToDataDir=None):
        if pathToDataDir:
            self.pathToDataDir = pathToDataDir

        csvDF = pd.read_csv(self.pathToData)

    def update(self, startDate = None, endDate = None):
        self.contracts = self.contracts.union(generateFutureContracts(startDate, endDate))

    def generateFutureContracts(self, startDate, endDate, commodityCode = None):
        if not commodityCode:
            commodityCode = self.code

        contractCodes = set()
        years = [str(startDate.year + i) for i in range(endDate.year - startDate.year + 1)]
        shortContractCodes = [commodityCode + str(month) for month in CONTRACT_MONTH_CODES]

        for year in years:
            for contractCode in shortContractCodes:
                fullContractCode = contractCode + year
                futureContract = FuturesContract(fullContractCode)
                contractCode.add(futureContract)
