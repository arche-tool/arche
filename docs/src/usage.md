### Usage Example ###

There are three commands available on Arche:

- `micro-features`: output basic processing of ANG files to be visualized on Paraview. Point properties, representing grain properties, such as orientation and phase will be shown in the `*.vtr` file. Line/surface properties, representing grain boundary properties, such as misorientation will be shown in the `*_faces.vtu` file.
```
arche micro-features -i ~/Desktop/lab/sample1.ang -o ~/Desktop/folder/baseName
```

- `optimum-OR`: find the actual orientation relationship between alpha grains and also provides the deviation from the theoretical Kurdjumov-Sachs `<1,1,2> 90-deg`
```
arche optimum-OR -i ~/Desktop/lab/sample1.ang
====================== Calculated ======================
Error: FitError {avgError = 2.5 deg, devError = 1.4 deg, maxError = 9.0 deg}
OR: [90,84,185] 90.4 deg
Direct deviation from KS: 2.8 deg
Deviation from (111) <-> (110): 0.7 deg
Deviation from [110] <-> [111]: 2.8 deg
```

- `reconstruction`: Reconstructs parent phase grains from siblings alpha grains. It's necessary to provide the phase ID of the phase as specified in the ANG file. And, in order to improve the accuracy, it's better to provide the actual orientation relationship calculated using `optimum-OR`
```
arche reconstruction --or "(90,84,185,90.4)" -g 1  -i ~/Desktop/lab/sample1.ang -o ~/Desktop/folder/baseName
```
