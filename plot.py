import matplotlib as mpl
mpl.use('Agg')
import matplotlib.pyplot as plt
from linecache import getlines as rd

class bands:
    def __init__(self, fname):
        self.raw_file = rd(fname)
        self.cont     = [ line.strip().split() for line in self.raw_file ]
        self.x        = self.cont[0]
        self.ys       = self.cont[1:]
        self.plot_out()

    def plot_out(self):
        for i in self.ys:
            plt.plot(self.x, i)
            pass
        plt.savefig("test.svg")
        pass



if __name__=="__main__":
    band_plot = bands("log")
    pass
