#!/usr/bin/env python3

from io import StringIO
from itertools import groupby, permutations, product
from math import cos, sin, radians
from sys import argv

import numpy as np

INTERSECTION_SIZE = 12
MATCHING_DIST_COUNT = (INTERSECTION_SIZE * (INTERSECTION_SIZE - 1)) / 2

SIN_90 = sin(radians(90))
COS_90 = cos(radians(90))
X_ROT = np.matrix([[1, 0, 0], [0, COS_90, -SIN_90], [0, SIN_90, COS_90]])
Y_ROT = np.matrix([[COS_90, 0, SIN_90], [0, 1, 0], [-SIN_90, 0, COS_90]])
Z_ROT = np.matrix([[COS_90, -SIN_90, 0], [SIN_90, COS_90, 0], [0, 0, 1]])


def point_diff(a, b):
    return tuple(coord_a - coord_b for (coord_a, coord_b) in zip(a, b))


def sq_dist(a, b):
    return sum(coord**2 for coord in point_diff(a, b))


def manhattan_dist(a, b):
    return sum(abs(coord) for coord in point_diff(a, b))


def group_len(grouper):
    return sum(1 for _ in grouper)


def get_dist_freqs(points):
    sq_distances = [sq_dist(a, b) for a, b in permutations(points, 2)]
    return {d: group_len(g) for d, g in groupby(sorted(sq_distances))}


def get_diffs(ref_points, points):
    pos_diffs = [point_diff(a, b) for a, b in product(ref_points, points)]
    return [(d, group_len(g)) for d, g in groupby(sorted(pos_diffs))]


def rotate_point_cloud(points, rot):
    rotated = np.rint(points.copy() @ rot.T)
    return np.array(rotated, dtype=np.int32)


def align_point_cloud(ref_points, points):
    for _ in range(4):
        for _ in range(4):
            for _ in range(4):
                diff = max(get_diffs(ref_points, points), key=lambda it: it[1])
                if diff[1] >= INTERSECTION_SIZE:
                    return diff[0], points

                points = rotate_point_cloud(points, Y_ROT)
            points = rotate_point_cloud(points, X_ROT)
        points = rotate_point_cloud(points, Z_ROT)

    raise Exception("Alignment not found")


class PointCloud:
    def __init__(self, points):
        self.points = points
        self.dist_freqs = get_dist_freqs(points)
        self.scanner_pos = (0, 0, 0)

    def does_merge_with(self, other_cloud):
        self_freq, other_freq = self.dist_freqs, other_cloud.dist_freqs
        common_dists = self_freq.keys() & other_freq.keys()
        if not common_dists:
            return False

        common_dist_count = sum(min(self_freq[k], other_freq[k]) for k in common_dists)
        return common_dist_count >= MATCHING_DIST_COUNT

    def merge_into(self, scanner):
        diff, points = align_point_cloud(scanner.points, self.points)
        self.scanner_pos = diff

        translated = np.add(points.copy(), np.array(diff))
        all_points = np.unique(np.append(scanner.points, translated, axis=0), axis=0)

        scanner.points = all_points
        scanner.dist_freqs = get_dist_freqs(all_points)


def parse_scanner_data(section):
    section_buffer = StringIO(section)
    points = np.loadtxt(section_buffer, skiprows=1, delimiter=",", dtype=np.int32)
    return PointCloud(points)


def merge_point_clouds(scanners):
    target, *rest = scanners
    done = []
    while rest:
        cloud = rest.pop(0)
        if cloud.does_merge_with(target):
            cloud.merge_into(target)
            done.append(cloud)
        else:
            rest.append(cloud)

    return target, done


def find_max_manhattan_dist(scanners):
    return max(
        manhattan_dist(a.scanner_pos, b.scanner_pos)
        for a, b in permutations(scanners, 2)
    )


with open(argv[1]) as input_file:
    sections = input_file.read().split("\n\n")
    point_clouds = [parse_scanner_data(section) for section in sections]

scanner_0, _ = merge_point_clouds(point_clouds)
print(f"Part 1: {scanner_0.points.shape[0]}")

max_distance = find_max_manhattan_dist(point_clouds)
print(f"Part 2: {max_distance}")
