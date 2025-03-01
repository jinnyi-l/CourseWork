import random
import math
import concurrent.futures
import time

def random_point(d):
    point = [random.uniform(-1, 1) for _ in range(d)]
    return point

def is_inside_hypersphere(point):
    distance = sum(x**2 for x in point)
    return distance <= 1

def calculate_hypersphere_volume(n, d, num_chunks):

    chunk_size = n // num_chunks
    total_inside_hypersphere_count = 0

    with concurrent.futures.ProcessPoolExecutor(max_workers=num_chunks) as executor:
        futures = []
        for _ in range(num_chunks):
            future = executor.submit(generate_and_count_points, chunk_size, d)
            futures.append(future)
        
        for future in concurrent.futures.as_completed(futures):
            inside_hypersphere_count = future.result()
            total_inside_hypersphere_count += inside_hypersphere_count

    volume_ratio = total_inside_hypersphere_count / n
    practical_volume = volume_ratio * (2**d)

    # Calculate theoretical volume
    theoretical_volume = (math.pi**(d/2)) / math.gamma(d/2 + 1)

    return practical_volume, theoretical_volume

def generate_and_count_points(chunk_size, d):
    random_points = [random_point(d) for _ in range(chunk_size)]
    inside_hypersphere_count = sum(map(is_inside_hypersphere, random_points))
    return inside_hypersphere_count

def main():
    n = int(input("The number of random points to generate(n): "))
    d = int(input("The number of dimensions (d): "))
    num_chunks = int(input("How many chunks to divide the data into: "))

    start_time = time.perf_counter()
    practical_volume, theoretical_volume = calculate_hypersphere_volume(n, d, num_chunks)
    end_time = time.perf_counter()

    print(f"Time taken: {end_time - start_time:.2f} seconds")
    print(f"Practical volume of a {d}-dimensional hypersphere: {practical_volume:.6f}")
    print(f"Theoretical volume of a {d}-dimensional hypersphere: {theoretical_volume:.6f}")

if __name__ == "__main__":
    main()
