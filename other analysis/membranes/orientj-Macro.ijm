// Macro to 
// Helena Crellin 


path = getInfo("image.directory");
// embryo_id only works if images saved in a folder with format like 20240528_E1_12ss
if (path.matches(".*(\\d{8}_E\\d{1}_\\d{2}ss).*")) {
	embryo_id = replace(path, ".*(\\d{8}_E\\d{1}_\\d{2}ss).*", "$1");
}else {
		embryo_id = replace(path, ".*(\\d{8}_E\\d{1}_NA).*", "$1");
}
run("Set Scale...", "distance=3.6363 known=1 unit=micron");
run("Clear Results");
run("OrientationJ Distribution", "tensor=4.0 gradient=0 radian=on table=on min-coherency=5.0 min-energy=5.0 ");
results_folder = "/Users/helena/Library/CloudStorage/OneDrive-UniversityofCambridge/PIV-Analysis/other analysis/membranes/results/orientj/";
saveAs("Results", results_folder + embryo_id + "_t29.csv");



open(path + "img_0036.tif");
run("Set Scale...", "distance=3.6363 known=1 unit=micron");
run("Clear Results");
run("OrientationJ Distribution", "tensor=4.0 gradient=0 radian=on table=on min-coherency=5.0 min-energy=5.0 ");
results_folder = "/Users/helena/Library/CloudStorage/OneDrive-UniversityofCambridge/PIV-Analysis/other analysis/membranes/results/orientj/";
saveAs("Results", results_folder + embryo_id + "_t36.csv");



run("Close All");

//run("Clear Results");
//run("Duplicate...", "duplicate");
//run("Subtract Background..."); // set radius to 100
////save(path + embryo_id + "_backsubtract");
//roiManager("Measure");
//
//results_folder = "/Users/helena/Library/CloudStorage/OneDrive-UniversityofCambridge/PIV-Analysis/other analysis/membranes/results/bg_subtracted/membrane/";
//saveAs("Results", results_folder + embryo_id + ".csv");

