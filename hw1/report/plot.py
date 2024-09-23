import matplotlib.pyplot as plt

def plot_single():
    # Read data from the file
    with open("experiment_single.dat", "r") as file:
        lines = file.readlines()

    x_values = []  # Delay time in ms
    y_values = []  # Average server response time for 100 requests

    for line in lines[1:]:
        values = list(map(int, line.split()))
        
        # First value is the delay time (x-axis)
        x = values[0]
        
        # Last three values are the response times, calculate their mean
        y = sum(values[1:]) / 3
        
        x_values.append(x)
        y_values.append(y)

    plt.figure(figsize=(10, 6))
    plt.plot(x_values, y_values, marker='o', linestyle='-')
    plt.xlabel("Delay time (ms)", fontsize=12)
    plt.ylabel("Average response time (µs)", fontsize=12)
    plt.title("Server response time vs. Delay time", fontsize=14)
    plt.grid(True)
    plt.show()

def plot_parallel():
    # Read data from the file
    with open("experiment_parallel.dat", "r") as file:
        lines = file.readlines()

    x_values = []  # Number of parallel processes (eq number of "machines")
    y_values = []  # Average server response time for 100 requests

    for line in lines[1:]:
        values = list(map(int, line.split()))
        
        # First value is the number of parallel processes (x-axis)
        x = values[0]
        
        # Last three values are the response times, calculate their mean
        y = sum(values[1:]) / 3
        
        x_values.append(x)
        y_values.append(y)

    plt.figure(figsize=(10, 6))
    plt.plot(x_values, y_values, marker='o', linestyle='-')
    plt.xlabel("Number of simultaneous machines", fontsize=12)
    plt.ylabel("Average response time (µs)", fontsize=12) 
    plt.title("Server tesponse time vs. Number of simultaneous machines", fontsize=14)
    plt.grid(True)
    plt.show()

def plot_further_handler(fileName, P, N):
    # Read data from the file
    with open(fileName, "r") as file:
        lines = file.readlines()

    x_values = []  # Number of parallel processes (eq number of "machines")
    y_values = []  # Average server response time for 100 requests

    for line in lines[1:]:
        values = list(map(int, line.split()))
        
        # First value is the number of parallel processes (x-axis)
        x = values[0]
        
        # Last three values are the response times, calculate their mean
        y = sum(values[1:]) / 3
        
        x_values.append(x)
        y_values.append(y)

    plt.figure(figsize=(10, 6))
    plt.plot(x_values, y_values, marker='o', linestyle='-')
    plt.xlabel("Number of handlers in the pool", fontsize=12)
    plt.ylabel("Average response time (µs)", fontsize=12) 
    plt.title("Server tesponse time for " + str(P) + " parallel machines requesting " + str(N) + " requests simultaneously vs. Number of handlers in the pool", fontsize=14)
    plt.grid(True)
    plt.show()

if __name__ == "__main__":
    plot_single()
    plot_parallel()
    plot_further_handler("experiment_increased_throughput_delay40_N10_P5.dat", 5, 10)
    plot_further_handler("experiment_increased_throughput_delay40_N100_P5.dat", 5, 100)

