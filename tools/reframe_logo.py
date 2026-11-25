#!/usr/bin/env python3
"""Re-cut man/figures/logo.png into the shared hex-sticker frame.

CDALabeller, AutoCDAScorer and structure-negative-steering are meant to read
as one set. This replaces the hexagon and keeps the artwork, so the brain, the
network and the title come through pixel for pixel inside the rim.

The frame is measured from CDALabeller's logo rather than guessed. Its hexagon
is 208 x 240 inside a 240 px square. 208/240 is cos(30), which is the width to
height ratio of a regular pointy-top hexagon, so its circumradius is 120.

The corners are arcs. Measuring CDALabeller's width 2, 4, 6, 10 and 16 px
below the top vertex gives 19, 26, 33, 46 and 66 px, against 7, 14, 21, 35 and
55 px for a sharp hexagon of the same size. Fitting an arc radius to that
profile gives r = 20 px, which reproduces every depth to within a pixel. The
rim is 8 px of #4c6433, which is already the most common rim pixel in both
this logo and CDALabeller's, so only the geometry changes.

Rounding a corner pulls the outline in along the bisector, which moves the top
and bottom vertices but not the left and right extremes, because those are
flat edges rather than vertices. A rounded regular hexagon is therefore always
a little wider than cos(30) times its height. The drawing is normalised to its
own bounding box to land on 208 x 240, which is what the reference did.

    python3 tools/reframe_logo.py

Needs Pillow and numpy. Not part of the package; tools/ is in .Rbuildignore.
"""
from __future__ import annotations

import math
from pathlib import Path

import numpy as np
from PIL import Image, ImageDraw

ROOT = Path(__file__).resolve().parent.parent
LOGO = ROOT / "man" / "figures" / "logo.png"

N = 240          # output canvas, square
HEX_W = 208      # hexagon width inside it, = N * cos(30)
RIM_W = 8        # rim thickness in px
CORNER = 20      # outer corner radius in px
RIM_RGB = (0x4C, 0x64, 0x33)
SS = 8           # supersampling, so the arcs do not stair-step


def rounded_hexagon(cx: float, cy: float, R: float, r: float,
                    per_arc: int = 48) -> list[tuple[float, float]]:
    """A pointy-top hexagon whose corners are circular arcs of radius r.

    Pointy-top puts a vertex at the top and the bottom, with the flat edges
    running down the left and right sides.
    """
    verts = [(cx + R * math.cos(math.radians(a)), cy - R * math.sin(math.radians(a)))
             for a in (90, 30, -30, -90, -150, 150)]
    pts: list[tuple[float, float]] = []
    n = len(verts)
    for i in range(n):
        p0, p1, p2 = verts[i - 1], verts[i], verts[(i + 1) % n]
        v1 = np.array(p0) - np.array(p1)
        v1 /= np.linalg.norm(v1)
        v2 = np.array(p2) - np.array(p1)
        v2 /= np.linalg.norm(v2)
        ang = math.acos(float(np.clip(v1 @ v2, -1.0, 1.0)))
        t1 = np.array(p1) + v1 * (r / math.tan(ang / 2.0))
        t2 = np.array(p1) + v2 * (r / math.tan(ang / 2.0))
        bis = v1 + v2
        bis /= np.linalg.norm(bis)
        c = np.array(p1) + bis * (r / math.sin(ang / 2.0))
        a1 = math.atan2(t1[1] - c[1], t1[0] - c[0])
        a2 = math.atan2(t2[1] - c[1], t2[0] - c[0])
        sweep = (a2 - a1 + math.pi) % (2 * math.pi) - math.pi
        for s in np.linspace(0.0, 1.0, per_arc):
            th = a1 + sweep * s
            pts.append((c[0] + r * math.cos(th), c[1] + r * math.sin(th)))
    return pts


def _fill(points, size) -> Image.Image:
    img = Image.new("L", (size, size), 0)
    ImageDraw.Draw(img).polygon(points, fill=255)
    return img


def frame_masks() -> tuple[np.ndarray, np.ndarray]:
    """(sticker, interior) masks at N x N, normalised to HEX_W x N."""
    size = N * SS
    c = size / 2.0
    R = (N / 2.0) * SS
    outer = _fill(rounded_hexagon(c, c, R, CORNER * SS), size)
    # Offsetting a regular polygon inward by d reduces its apothem by d, and a
    # hexagon's apothem is R*cos(30), so the circumradius drops by d/cos(30).
    d = RIM_W * SS
    inner = _fill(rounded_hexagon(c, c, R - d / math.cos(math.radians(30)),
                                  max(CORNER * SS - d, 1)), size)

    a = np.asarray(outer)
    ys, xs = np.nonzero(a > 128)
    box = (int(xs.min()), int(ys.min()), int(xs.max()) + 1, int(ys.max()) + 1)

    def norm(img: Image.Image) -> np.ndarray:
        cropped = img.crop(box).resize((HEX_W, N), Image.LANCZOS)
        out = Image.new("L", (N, N), 0)
        out.paste(cropped, ((N - HEX_W) // 2, 0))
        return np.asarray(out)

    return norm(outer), norm(inner)


def main() -> None:
    src = Image.open(LOGO).convert("RGBA")
    if src.size != (N, N):
        src = src.resize((N, N), Image.LANCZOS)
    sa = np.asarray(src).astype(int)

    # The sticker interior is black, so the artwork is flattened onto black.
    # Flattening onto white leaves a pale fringe on every antialiased edge.
    black = Image.new("RGBA", src.size, (0, 0, 0, 255))
    flat = np.asarray(Image.alpha_composite(black, src).convert("RGB")).astype(int)

    outer_a, inner_a = frame_masks()
    sticker = outer_a > 8
    interior = inner_a > 128

    # The source's own sticker, from its alpha. Its white title must not be
    # mistaken for background, which is why this does not threshold on colour.
    s_inside = sa[..., 3] > 16
    for _ in range(RIM_W + 2):   # step in past its own rim
        s_inside = (s_inside
                    & np.roll(s_inside, 1, 0) & np.roll(s_inside, -1, 0)
                    & np.roll(s_inside, 1, 1) & np.roll(s_inside, -1, 1))

    out = np.zeros((N, N, 4), dtype=np.uint8)
    out[sticker] = [*RIM_RGB, 255]
    out[interior] = [0, 0, 0, 255]
    keep = interior & s_inside
    out[keep, :3] = flat[keep]
    out[keep, 3] = 255
    # The old frame was a sharper hexagon, so a sliver of its rim can fall
    # inside the new one. That sliver becomes interior. The test is greenness
    # rather than distance to the rim colour: the interior carries a faint
    # #2e2d2d ring just inside the old rim, which sits 91 away from #4c6433
    # and was being blacked out by a distance threshold.
    r_, g_, b_ = flat[..., 0], flat[..., 1], flat[..., 2]
    old_rim = keep & (g_ > r_ + 15) & (g_ > b_ + 15) & (g_ > 50)
    out[old_rim] = [0, 0, 0, 255]
    # Keep the frame's antialiased outline rather than a stair-stepped one.
    edge = (outer_a > 0) & (outer_a < 250) & ~interior
    out[edge, :3] = RIM_RGB
    out[edge, 3] = outer_a[edge]

    Image.fromarray(out).save(LOGO)
    print(f"wrote {LOGO}")


if __name__ == "__main__":
    main()
