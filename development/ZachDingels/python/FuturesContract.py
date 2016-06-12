import quandl

class FuturesContract():
    """docstring for FuturesContract"""
    def __init__(self, contractCode = None):
        self.contractCode = contractCode
        self.update()

    def update(self):
        self.data = quandl.get(self.contractCode)
