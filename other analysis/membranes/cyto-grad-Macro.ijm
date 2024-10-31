// Macro to measure intensities of membrane ROIs and save results in tidy format
// Helena Crellin 22/07/24
// image to analyse open and rois saved as membrane-grad.zip

path = getInfo("image.directory");
// embryo_id only works if images saved in a folder with format like 20240528_E1_12ss
if (path.matches(".*(\\d{8}_E\\d{1}_\\d{2}ss).*")) {
	embryo_id = replace(path, ".*(\\d{8}_E\\d{1}_\\d{2}ss).*", "$1");
}else {
		embryo_id = replace(path, ".*(\\d{8}_E\\d{1}_NA).*", "$1");
}

//open(path + "cyto-grad.zip");
roiManager("Open", path + "cyto-grad.zip");

run("Clear Results");

run("Set Measurements...", "area mean standard min median display redirect=None decimal=3");
run("Set Scale...", "distance=3.6363 known=1 unit=micron");

count = roiManager("count");
array = newArray(count);
  for (i=0; i<array.length; i++) {
      array[i] = i;
  }
roiManager("select", array);


roiManager("Measure");

results_folder = "/Users/helena/Library/CloudStorage/OneDrive-UniversityofCambridge/PIV-Analysis/other analysis/membranes/results/cyto-grad/";
//File.makeDirectory(results_folder);
saveAs("Results", results_folder + embryo_id + ".csv");

//run("Clear Results");
//run("Duplicate...", "duplicate");
//run("Subtract Background..."); // set radius to 100
////save(path + embryo_id + "_backsubtract");
//roiManager("Measure");
//
//results_folder = "/Users/helena/Library/CloudStorage/OneDrive-UniversityofCambridge/PIV-Analysis/other analysis/membranes/results/bg_subtracted/membrane/";
//saveAs("Results", results_folder + embryo_id + ".csv");

roiManager("Deselect");
roiManager("Delete");
//close();

