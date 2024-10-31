function tiffStack=openTiffStack(fileName)
    TifFile=fileName;
    InfoImage=imfinfo(TifFile);
    width=InfoImage(1).Width;
    height=InfoImage(1).Height;
    depth=length(InfoImage);
    tiffStack=zeros(height,width,depth,'double');

    for i=1:depth
       tiffStack(:,:,i)=imread(TifFile,'Index',i,'Info',InfoImage);
    end
end