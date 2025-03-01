import random
import math
import time

def random_point(d):
    point = [random.uniform(-1, 1) for _ in range(d)]
    return point

def distance_d(point):
    distance_squared = sum(x**2 for x in point)
    distance = math.sqrt(distance_squared)
    return distance

def is_inside_hypersphere(point):
    distance = distance_d(point)
    return distance <= 1

def hypersphere_volume(n, d):
    start_time = time.perf_counter()

    random_points = [random_point(d) for _ in range(n)]
    inside_hypersphere_points = filter(is_inside_hypersphere, random_points)
    inside_hypersphere_count = len(list(inside_hypersphere_points))

    volume_ratio = inside_hypersphere_count / n
    theory = volume_ratio * (2**d)
    result = (math.pi**(d/2)) / math.gamma(d/2 + 1)
    print('The practical approximation is: ' + str(theory))
    print('The theoretical resultpractical approximation is: ' + str(result))

    end_time = time.perf_counter()
    processing_time = end_time - start_time
    print(f"Time taken: {processing_time:.6f} seconds")

    return theory

def main():
    n = int(input("The number of random points to generate(n): "))
    d = int(input("The number of dimensions (d): "))

    estimated_volume = hypersphere_volume(n, d)

if __name__ == "__main__":
    main()
