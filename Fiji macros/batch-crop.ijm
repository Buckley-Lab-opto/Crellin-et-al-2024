macro "Batch resize and crop" {	
	input = getDirectory("Choose directory");
	extension = "tif"; // or nd
	parent = File.getParent(input);
	output = parent+File.separator+"cropped"; 
		if (!File.isDirectory(output)) {
		 		File.makeDirectory(output);
		}	

   setBatchMode(true);
   count = 0;
   count.files(input);
   n = 0;
   process.files(input);

	   function count.files(input) {
	      list = getFileList(input);
	      for (i=0; i<list.length; i++) {
	          if (endsWith(list[i], "/"))
	              count.files(""+input+list[i]);
	          else
	              count++;
	      }
	  }
	
	   function process.files(input) {
	      list = getFileList(input);
	      for (i=0; i<list.length; i++) {
	          if (endsWith(list[i], "/"))
	              process.files(""+input+list[i]);
	          else {
	             showProgress(n++, count);
	             path = input+list[i];
	             process.file(path);
	          }
	      }
	  }
	
	  function process.file(path) {
	       if (endsWith(path, "."+extension)) {
	       	run("Bio-Formats Macro Extensions");
			Ext.setId(path);
			print("Opening "+path);
		    run("Bio-Formats Importer", "open=["+path+"] color_mode=Default view=Hyperstack stack_order=XYCZT");
			fileName = File.getNameWithoutExtension(path);
			print("Cropping "+fileName);
			run("Set Scale...", "distance=3.6363 known=1 unit=micron");
			run("Duplicate...", "title=["+fileName+"_crop] duplicate" );
			makeRectangle(150, 150, 900, 900);
			run("Crop");
			
			print("Saving "+fileName);
			
			saveAs("tiff", output+File.separator+fileName+"_crop"+".tif");

//			selectWindow(fileName);
//			print("Cropping RFP "+fileName);
//			makeRectangle(1350, 150, 900, 900);
//			run("Crop");
//			print("Saving RFP "+fileName);
//			saveAs("tiff", output+File.separator+fileName+"_RFP"+".tif");
			
			close("*");
	      }
	  }
}