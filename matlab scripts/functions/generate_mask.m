function gen_mask = generate_mask(maskiererx, maskierery, ensemble_i1, xroi, yroi, image1_roi)
    %get mask from mask list
	ximask={};
	yimask={};
	if size(maskiererx,2)>=ensemble_i1
		for j=1:size(maskiererx,1)
			if isempty(maskiererx{j,ensemble_i1})==0
				ximask{j,1}=maskiererx{j,ensemble_i1}; %#ok<*AGROW>
				yimask{j,1}=maskierery{j,ensemble_i1};
			else
				break
			end
		end
		if size(ximask,1)>0
			mask_inpt=[ximask yimask];
		else
			mask_inpt=[];
		end
	else
		mask_inpt=[];
	end
	if numel(mask_inpt)>0
		cellmask=mask_inpt;
		mask=zeros(size(image1_roi));
		for i=1:size(cellmask,1)
			masklayerx=cellmask{i,1};
			masklayery=cellmask{i,2};
			mask = mask + poly2mask(masklayerx-xroi,masklayery-yroi,size(image1_roi,1),size(image1_roi,2)); %kleineres eingangsbild und maske geshiftet
		end
	else
		mask=zeros(size(image1_roi));
	end
	mask(mask>1)=1;
	gen_mask = mask;
end