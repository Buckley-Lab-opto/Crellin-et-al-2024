// Macro to measure line profiles from ROI manager in 2 channel image and save results in tidy format
// Helena Crellin 28/05/24
// start with line ROIs in ROI manager and image to analyse open

path = getInfo("image.directory");
file = getInfo("image.title");
// date only works if images saved in a folder with date format like 2024-05-28
date = replace(path, ".*(\\d{4}\\-\\d{2}\\-\\d{2}).*", "$1");

// run 2 lines below if new set of rois
roiManager("remove slice info")
roiManager("save", path + File.separator + date + "_" + file + "_line-rois.zip");

run("Clear Results");
run("Subtract Background...") // set radius to 100
save(path + file + "_backsubtract");

getPixelSize(unit, pixelWidth, pixelHeight);


//slice = getSliceNumber();
//	print("Slice: " + slice + ", ROI: " + i + ", Profile: " + profile);

r = 0;
setSlice(1); 
for (i=0; i<RoiManager.size; i++) { 
	roiManager("Select", i);
	Roi.setStrokeWidth(7); 
	profile = getProfile();
	for (j=0; j<profile.length; j++){
		setResult("roi", r, i);
		setResult("distance ("+unit+")", r, j*pixelHeight);
		setResult("intensity", r, profile[j]);
		setResult("channel", r, "1");
		setResult("file", r, file);
		setResult("date", r, date);
		updateResults();	
		r++;
	}
}

setSlice(2);
for (i=0; i<RoiManager.size; i++) { 
	roiManager("Select", i);
	Roi.setStrokeWidth(7);
	profile = getProfile();
	for (j=0; j<profile.length; j++){
		setResult("roi", r, i);
		setResult("distance ("+unit+")", r, j*pixelHeight);
		setResult("intensity", r, profile[j]);
		setResult("channel", r, "2");
		setResult("file", r, file);
		setResult("date", r, date);
		updateResults();	
		r++;
	}
}


results_folder = "/Users/helena/Library/CloudStorage/OneDrive-UniversityofCambridge/light patterning drive/data/biosensor to analyse/snaps/results/bg-subtracted/"
saveAs("Results", results_folder + date +"_"+ file +".csv");

