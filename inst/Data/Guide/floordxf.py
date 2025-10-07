import cv2
import ezdxf
import pytesseract

# Load the image
import os

img_path = "/Users/simonepernice/Desktop/GIT/R_packages_project/TrustAlert/FORGE4FLAME/inst/Data/Guide/image.png"
image = cv2.imread(img_path)

if image is None:
    raise FileNotFoundError(f"Image not found at: {img_path}")

print("Image loaded")
gray = cv2.cvtColor(image, cv2.COLOR_BGR2GRAY)
_, thresh = cv2.threshold(gray, 240, 255, cv2.THRESH_BINARY_INV)

print("Find rectangles (rooms)")
contours, _ = cv2.findContours(thresh, cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_SIMPLE)


print(f"Contours found: {len(contours)}")

doc = ezdxf.new()
msp = doc.modelspace()

for cnt in contours:
    approx = cv2.approxPolyDP(cnt, 0.02 * cv2.arcLength(cnt, True), True)
    if len(approx) == 4 and cv2.contourArea(cnt) > 500:
        x, y, w, h = cv2.boundingRect(cnt)
        msp.add_lwpolyline([(x, -y), (x + w, -y), (x + w, -y - h), (x, -y - h)], close=True)


# Save it
doc.saveas("generated_floorplan.dxf")
