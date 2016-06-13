import quandl

class FuturesContract():
    """docstring for FuturesContract"""
    def __init__(self, contractCode = None):
        self.contractCode = contractCode
        self.makeQuandlCode()
        self.update()

    def __str__(self):
        return self.quandlCode + ': ' + str(self.data.shape)

    def makeQuandlCode(self):
        ICEExchange = ['CC','KC','CT','OJ','SB']
        if self.contractCode[0:2] in ICEExchange:
            self.quandlCode = 'ICE/%s' % self.contractCode
        else:
            self.quandlCode = 'CME/%s' % self.contractCode

    def update(self):
        self.data = quandl.get(self.quandlCode)
