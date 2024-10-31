function plot_cumulative_strain_rate(image_path, mask_file, piv_path, path2roi, out_folder, param)

    pixel_s=param.pixel_s; % [um (/pix)]
    time_step=param.time_step; %[s (/timePoint)]
    spatial_filt_size = param.spatial_filt_size;
    time_filt_size = param.time_filt_size;
    initial_time = param.initial_time;
    final_time = param.final_time;
    max_value = param.max_value;
    min_value = param.min_value;
    
    xroi = param.xroi;
    yroi = param.yroi;
    
    S = openTiffStack(image_path);
    S = uint8(rescale(S, 0, 255));

    load(mask_file, 'maskiererx', 'maskierery')
    %% preprocess vf
    vf = load(piv_path);
    
    x = vf.x{1};
    y = vf.y{1};
    
    % Set nan values to zeros.
    for v_i=1:length(vf.u_original)
        vf.u_original{v_i}(isnan(vf.u_original{v_i}))=0;
        vf.v_original{v_i}(isnan(vf.v_original{v_i}))=0;
    end
    
    averaging_filter=ones([spatial_filt_size spatial_filt_size time_filt_size])/spatial_filt_size^2/time_filt_size;
    
    u=zeros([size(vf.u_original{1}) length(vf.u_original)]);
    v=zeros([size(vf.u_original{1}) length(vf.u_original)]);
    for i_ind=1:size(u,3)-1
    
        u(:,:,i_ind)=vf.u_original{i_ind}*pixel_s/time_step; 
        v(:,:,i_ind)=vf.v_original{i_ind}*pixel_s/time_step;
    end
    
    u = imfilter(u, averaging_filter, 'replicate');
    v = imfilter(v, averaging_filter, 'replicate');
    
    % Generate spatial derivative filter. Divide by width of stencil in um units.
    derivative_filter_s=3;
    derivative_filter=zeros(1,derivative_filter_s);
    derivative_filter([1 end])=[1 -1]/((x(1,2)-x(1,1))*(derivative_filter_s-1)*pixel_s); % [1 -1]/length of spatial avrg
    
    % Compute spatial derivatives. Result of this will have unit 1/min.
    dudx=gather(convn(u,derivative_filter,'same')); %_ all -> all time points
    dudy=gather(convn(u,derivative_filter','same'));
    dvdx=gather(convn(v,derivative_filter,'same'));
    dvdy=gather(convn(v,derivative_filter','same'));
    
    disp('done');
    %%
    aniStrain = zeros(size(dudx,1),size(dudx,2),size(dudx,3));
    aniDirs = ones(size(dudx,1),size(dudx,2),2,size(dudx,3));
    isoStrain = zeros(size(dudx,1),size(dudx,2),size(dudx,3));
    totStrain = zeros(size(dudx,1),size(dudx,2),size(dudx,3));
    
    
    for t = 1:size(u,3)
        a = dudx(:,:,t)-dvdy(:,:,t);
        b = dudy(:,:,t)+dvdx(:,:,t);
        eig = sqrt(a.^2+b.^2);
        TSt = sqrt(dudx(:,:,t).^2 + b.^2 + b.^2 + dvdy(:,:,t).^2);
        ASt = 0.5*eig;
        ISt = 0.5*(dudx(:,:,t) + dvdy(:,:,t));
    
%         totStrain(:,:,t) = sqrt(dudx(:,:,t).^2 + dudy(:,:,t).^2 + dvdx(:,:,t).^2 + dvdy(:,:,t).^2);
        aniStrain(:,:,t) = ASt;
        isoStrain(:,:,t) = ISt;
        totStrain(:,:,t) = TSt;
    
        % y component of the contracting eigenvector for x component = 1
        aniDirs(:,:,2,t) = (-eig-0.5*a)./(0.5*b);
    
        aniDirs(:,:,1,t) = aniDirs(:,:,1,t)./sqrt(1 + aniDirs(:,:,2,t).^2);
        aniDirs(:,:,2,t) = aniDirs(:,:,2,t)./sqrt(1 + aniDirs(:,:,2,t).^2);
    
    
    end
    disp('done');
    
    %% plot results
    
   
    crop = floor(spatial_filt_size/2)+1;
    
    roi = imread(path2roi);
    roi = roi(y(crop,2):y(end-crop,2), x(1,crop):x(1,end-crop));
    roi_perim = bwperim(roi);
    roi_perim = imdilate(roi_perim, strel('disk', 4));
    
    %% get background image
    I = S(:,:,final_time);

    
    clf
    
    M = ~logical(generate_mask(maskiererx, maskierery, final_time, xroi, yroi, I));
    M = M(y(crop,2):y(end-crop,2), x(1,crop):x(1,end-crop));
    M = imresize(M, [size(u,2), size(u,1)]);

    I = I(y(crop,2):y(end-crop,2), x(1,crop):x(1,end-crop));
    
    I = imadjust(I);
    I(roi_perim(:)) = 0;



    %% cumulative total strain


    clf
    
    I2 = I;
    I2 = imadjust(I2);
    alpha = 0.5;
    
    imagesc(x(1,crop:end-crop),y(crop:end-crop,1), I2);

    set(gca,'xtick',[])
    set(gca,'ytick',[])
    axis image;

    set(gca, 'YDir','reverse'); set(gca, 'FontSize', 11)
    colormap('gray');
    freezeColors

    str = sum(totStrain(:,:,initial_time:final_time),3).*M;

    
    
    hold on
    imagesc(x(1,crop:end-crop), y(crop:end-crop,1), str(crop:end-crop,crop:end-crop),'AlphaData', alpha)

    colLim = max_value;%max(str(:));
    caxis([min_value colLim])
    c = colorbar;
    c.Label.String = '1 / s';
    c.Ticks = [ 0 colLim];
    c.TickLabels = num2str([0 colLim]');

    box on

    set(gca,'xtick',[])
    set(gca,'ytick',[])
    axis image;
    cmap = cmocean('amplitude');
    %cmap = viridis;
    colormap(gca, cmap)

    set(gca, 'YDir','reverse'); set(gca, 'FontSize', 11)
    addScaleBar(pixel_s, 50)

    title('cumulative strain')


    export_fig([out_folder filesep 'cum_str_' num2str(initial_time, '%04d') '_' num2str(final_time, '%04d') '.png']);    


    %% cumulative iso

    clf
    
    I2 = I;
    I2 = imadjust(I2);
    alpha = 0.5;
    
    imagesc(x(1,crop:end-crop),y(crop:end-crop,1), I2);

    set(gca,'xtick',[])
    set(gca,'ytick',[])
    axis image;

    set(gca, 'YDir','reverse'); set(gca, 'FontSize', 11)
    colormap('gray');
    freezeColors

    iso = sum(isoStrain(:,:,initial_time:final_time).*M, 3);
    
    
    hold on
    imagesc(x(1,crop:end-crop), y(crop:end-crop,1), iso(crop:end-crop,crop:end-crop),'AlphaData', alpha)

    colLim = max_value/2;
    caxis([-colLim colLim])
    c = colorbar;
    c.Label.String = '1 / s';
    c.Ticks = [-colLim 0 colLim];
    c.TickLabels = num2str([-colLim 0 colLim]'); 

    box on

    set(gca,'xtick',[])
    set(gca,'ytick',[])
    axis image;
    cmap = cmocean('balance');
    colormap(gca, cmap)

    set(gca, 'YDir','reverse'); set(gca, 'FontSize', 11)
    addScaleBar(pixel_s, 50)

    title('Isotropic part (strain rate)')

    export_fig([out_folder filesep 'cum_iso_' num2str(initial_time, '%04d') '_' num2str(final_time, '%04d') '.png']);    

    %% cumulative ani strain    
    clf
    I2 = I;
    I2 = imadjust(I2);
    alpha = 0.5;
    
    imagesc(x(1,crop:end-crop),y(crop:end-crop,1), I2);

    set(gca,'xtick',[])
    set(gca,'ytick',[])
    axis image;

    set(gca, 'YDir','reverse'); set(gca, 'FontSize', 11)
    colormap('gray');
    freezeColors
    
    arrowSkip = 2;

    sk = 1;
    kernel = ones(sk)./sk^2;

    ani = sum(aniStrain(:,:,initial_time:final_time).*M,3);
   
    
    dirs = aniDirs(:,:,:,final_time);
    dirs(:,:,1) = imfilter(dirs(:,:,1), kernel);
    dirs(:,:,2) = imfilter(dirs(:,:,2), kernel);
    
    hold on
    imagesc(x(1,crop:end-crop), y(crop:end-crop,1), ani(crop:end-crop,crop:end-crop),'AlphaData', alpha)
    colLim = max_value/2;
    
    c = colorbar;
    c.Ticks = [0 colLim/2 colLim];
    c.TickLabels = num2str([0 colLim/2 colLim]'); 
    c.Label.String = '1 / h';
    caxis([min_value colLim])
    %cmap = viridis;
    cmap = cmocean('amp');
    colormap(gca, cmap)
      
    box on

    set(gca,'xtick',[])
    set(gca,'ytick',[])
    axis image;

    set(gca, 'YDir','reverse'); set(gca, 'FontSize', 11)

    title('Anisotropic part (strain rate)')
    addScaleBar(pixel_s, 50)

    
    export_fig([out_folder filesep 'cum_ani_' num2str(initial_time, '%04d') '_' num2str(final_time, '%04d') '.png']);    



end