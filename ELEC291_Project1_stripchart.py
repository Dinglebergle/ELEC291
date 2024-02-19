import numpy as np
import matplotlib.pyplot as plt
import matplotlib.animation as animation
import sys, time, math
import serial

xsize = 400
ysize = 250
def data_gen():
    t = data_gen.t
    while True:
        t += 1
        strin = ser.readline()
        decoded = strin.decode('utf-8')
        val = float(decoded)
        yield t, val

def run(data):
    # update the data
    t, y = data
    if t > -1:
        xdata.append(t)
        ydata.append(y)

        if t > xsize:  # Scroll to the left.
            ax.set_xlim(t - xsize, t)
        line.set_data(xdata, ydata)

        # Calculate and display the current average
        avg = np.mean(ydata)
        max = np.max(ydata)
        min = np.min(ydata)
        avg_line.set_data([xdata[0], xdata[-1]], [avg, avg])
        ax.legend([f'Temperature: {y:.2f}°C', f"Average: {avg:.2f}°C"])
        ax.set_title(f"Reflow Temperature Data          Max: {max:.2f}°C ||| Min: {min:.2f}°C")

    return line, avg_line

def on_close_figure(event):
    sys.exit(0)

ser = serial.Serial(
    port='COM8',
    baudrate=115200,
    parity=serial.PARITY_NONE,
    stopbits=serial.STOPBITS_TWO,
    bytesize=serial.EIGHTBITS
)

ser.isOpen()

data_gen.t = -1
fig = plt.figure()
fig.canvas.mpl_connect('close_event', on_close_figure)
ax = fig.add_subplot(111)
line, = ax.plot([], [], lw=2)
avg_line, = ax.plot([], [], lw=0.5, color='red', marker='.')  # Average temperature line
ax.set_ylim(18, ysize)
ax.set_xlim(0, xsize)
ax.grid()
xdata, ydata = [], []

# Important: Although blit=True makes graphing faster, we need blit=False to prevent
# spurious lines from appearing when resizing the stripchart.
ani = animation.FuncAnimation(fig, run, data_gen, blit=False, interval=100, repeat=False)
plt.xlabel("Time")
plt.ylabel("Temperature (°C)")
plt.title("Reflow Temperature Data")
#plt.legend(['Temperature', 'Average'])
plt.show()
