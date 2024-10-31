macro "Batch save image sequence" {
	input = getDirectory("Choose directory");
	extension = "tif";
	parent = File.getParent(input);
	output = parent+File.separator+"data"; 
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
			print("Saving image sequence of "+fileName);
			
			output = parent+File.separator+"data"; 
			output = output+File.separator+fileName; 
			if (!File.isDirectory(output)) {
		 		File.makeDirectory(output);
		 		}	
		 	// print(output);
		 	output = output+File.separator+"image-sequence"; 
			if (!File.isDirectory(output)) {
		 		File.makeDirectory(output);
		 		}	
		 	print(output);
			
			run("Image Sequence... ", "select=["+path+"] dir=["+output+"] format=TIFF name=img_ start=1");

			close("*");
			
	      }
	  }
}