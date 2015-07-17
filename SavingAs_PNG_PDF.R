## Graphics Devices in Windows

windows()     # open new graphics device window in windows
dev.cur()     # returns number of currnet graphics device that is being used

with(faithful, hist(faithful[,1], breaks = 10))  # plot a histogram in current graphics device

windows(<integer >= 1>)    # select an open graphics device or open a new one with the input integer

with(faithful, plot(faithful[,1], faithful[,2]))        # plot graph in current device

## Copy plot to PNG
dev.copy(png, file = "plot2.png")
dev.off()

### Copy to PDF
pdf(file = "myplot.pdf")
with(faithful, plot(eruptions, waiting))
title(main = "Old Faithful Geyser Data")
dev.off()

