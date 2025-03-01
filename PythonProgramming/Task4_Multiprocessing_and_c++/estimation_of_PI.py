

import random
import math
import matplotlib.pyplot as plt

def generate_points(n):
    inside_circle = 0
    points_x = []
    points_y = []

    for _ in range(n):
        x = random.uniform(0, 1)
        y = random.uniform(0, 1)
        distance = math.sqrt((x - 0.5)**2 + (y - 0.5)**2)  # Distance to the center (0.5, 0.5)

        if distance <= 0.5:  # Check if the distance is less than or equal to the radius (0.5)
            inside_circle += 1
            points_x.append(x)
            points_y.append(y)
        else:
            points_x.append(x)
            points_y.append(y)

    return inside_circle, points_x, points_y #Return the num of points that are inside circle and their coodinates

def main(n):
    inside_circle, points_x, points_y = generate_points(n)

    approx_pi = 4 * inside_circle / n

    print("Number of points inside the circle (nc):", inside_circle)
    print("Approximation of PI:", approx_pi)

    distances = [math.sqrt((x - 0.5)**2 + (y - 0.5)**2) for x, y in zip(points_x, points_y)] #zip () function make the coordinate into a tuple

    colors = ['red' if distance <= 0.5 else 'blue' for distance in distances]

    plt.scatter(
        points_x,
        points_y,
        c=colors
    )

    plt.xlabel('X')
    plt.ylabel('Y')
    plt.title('Monte Carlo Simulation of π')
    plt.gca().set_aspect('equal', adjustable='box')


    plt.xlim(0, 1)
    plt.ylim(0, 1)
    
    plt.show()

if __name__ == "__main__":
    n = int(input("Enter the number of random points to generate: "))
    main(n)
