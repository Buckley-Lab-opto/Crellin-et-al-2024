function plot_piv_results(image_path, mask_file, piv_path, path2roi, out_folder, param, experiment_folder)

% Helena edited on 19/12/23 to change file saving name of plots to show expname
% Helena edited to 15/05/24 to add iso x and y
% Helena edited 15/05/24 to add guillermo's AP / ML str

%%
    pixel_s=param.pixel_s; % [um (/pix)]
    time_step=param.time_step; %[s (/timePoint)]
    spatial_filt_size = param.spatial_filt_size;
    time_filt_size = param.time_filt_size;
    on_time_point = param.on_time_point;
    off_time_point = param.off_time_point;

    expname = strsplit(experiment_folder, '/');
    expdate = expname{end-1};
    expname = strsplit(expname{end}, '.tif');
    expname = expname{1};

    xroi = param.xroi;
    yroi = param.yroi;
    
    S = openTiffStack(image_path);
    S = uint8(rescale(S, 0, 255));

    load(mask_file, 'maskiererx', 'maskierery');
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
    isoStrainx = zeros(size(dudx,1),size(dudx,2),size(dudx,3));
    isoStrainy = zeros(size(dudx,1),size(dudx,2),size(dudx,3));
    totStrain = zeros(size(dudx,1),size(dudx,2),size(dudx,3));
    
    
    for t = 1:size(u,3)
        a = dudx(:,:,t)-dvdy(:,:,t);
        b = dudy(:,:,t)+dvdx(:,:,t);
        eig = sqrt(a.^2+b.^2); % eigenvalues of anisotropic part is 1/2 +- this 
        TSt = sqrt(dudx(:,:,t).^2 + b.^2 + b.^2 + dvdy(:,:,t).^2); % Should it be (0.5 * b)^2 + (0.5 * b)^2, if \xi (symmetrised strain rate) and should it be sqrt of sq?
        % line above calculating the l2-norm / euclidian norm of the velocity matrix; a summary of its magnitude  
        ASt = 0.5*eig; % eigenvalue of anisotropic part from Rozbicki et al 2015
        ISt = 0.5*(dudx(:,:,t) + dvdy(:,:,t)); %   eigenvalues of isotropic decomposed part - half the strain trace
        IStx = dudx(:,:,t); 
        ISty = dvdy(:,:,t); 


        % totStrain(:,:,t) = sqrt(dudx(:,:,t).^2 + dudy(:,:,t).^2 +
        % dvdx(:,:,t).^2 + dvdy(:,:,t).^2) - non-symmetrised (L)
        aniStrain(:,:,t) = ASt;
        isoStrain(:,:,t) = ISt;
        isoStrainx(:,:,t) = IStx;
        isoStrainy(:,:,t) = ISty;
        totStrain(:,:,t) = TSt;
    
        % y component of the contracting eigenvector for x component = 1
        aniDirs(:,:,2,t) = (-eig-0.5*a)./(0.5*b);
    
        aniDirs(:,:,1,t) = aniDirs(:,:,1,t)./sqrt(1 + aniDirs(:,:,2,t).^2);
        aniDirs(:,:,2,t) = aniDirs(:,:,2,t)./sqrt(1 + aniDirs(:,:,2,t).^2);
    
    
    end
    disp('done');

    % get ap and ml strain
    totStrain = zeros(size(dudx,1),size(dudx,2),size(dudx,3));
    apStrain = zeros(size(dudx,1),size(dudx,2),size(dudx,3));
    mlStrain = zeros(size(dudx,1),size(dudx,2),size(dudx,3));

    for t = 1:size(u,3)
        % Compute spatial derivatives. Result of this will have unit 1/min.
        dudx=gather(convn(u,derivative_filter,'same')); %_ all -> all time points
        dudy=gather(convn(u,derivative_filter','same'));
        dvdx=gather(convn(v,derivative_filter,'same'));
        dvdy=gather(convn(v,derivative_filter','same'));
        b = dudy(:,:,t)+dvdx(:,:,t);
        TSt = sqrt(dudx(:,:,t).^2 + b.^2 + b.^2 + dvdy(:,:,t).^2); 
        totStrain(:,:,t) = TSt;

        % Compute spatial derivatives. Result of this will have unit 1/min.
        dudx=gather(convn(u*0,derivative_filter,'same')); %_ all -> all time points
        dudy=gather(convn(u*0,derivative_filter','same'));
        dvdx=gather(convn(v,derivative_filter,'same'));
        dvdy=gather(convn(v,derivative_filter','same'));
        b = dudy(:,:,t)+dvdx(:,:,t);
        TSt = sqrt(dudx(:,:,t).^2 + b.^2 + b.^2 + dvdy(:,:,t).^2); 
        % b = dudy(:,:,t)+dvdx(:,:,t); if we want TSt of AP why not do norm of dv/dy (normal strain - expansion/contraction) and dv/dx (shear strain )  
        apStrain(:,:,t) = TSt;

        % Compute spatial derivatives. Result of this will have unit 1/min.
        dudx=gather(convn(u,derivative_filter,'same')); %_ all -> all time points
        dudy=gather(convn(u,derivative_filter','same'));
        dvdx=gather(convn(v*0,derivative_filter,'same'));
        dvdy=gather(convn(v*0,derivative_filter','same'));
        b = dudy(:,:,t)+dvdx(:,:,t);
        TSt = sqrt(dudx(:,:,t).^2 + b.^2 + b.^2 + dvdy(:,:,t).^2);
        mlStrain(:,:,t) = TSt;
    end
    disp('done');

    
    %% plot results
    extend = @(x) x(:);
    
    surf_folder = [out_folder filesep 'surface'];
    vel_folder = [out_folder filesep 'vel'];
    iso_folder = [out_folder filesep 'iso'];
    iso_x_folder = [out_folder filesep 'iso_x'];
    iso_y_folder = [out_folder filesep 'iso_y'];
    ani_folder = [out_folder filesep 'ani'];
    str_folder = [out_folder filesep 'str'];
    apstr_folder = [out_folder filesep 'ap_str'];
    mlstr_folder = [out_folder filesep 'ml_str'];
    cum_str_folder = [out_folder filesep 'cumulative_str'];
    cum_iso_folder = [out_folder filesep 'cumulative_iso'];
    cum_ani_folder = [out_folder filesep 'cumulative_ani'];
    angle_folder = [out_folder filesep 'ang'];
    angmap_folder = [out_folder filesep 'angmap'];
    
    mkdir(surf_folder);
    mkdir(vel_folder);
    mkdir(iso_folder);
    mkdir(iso_x_folder);
    mkdir(iso_y_folder);
    mkdir(ani_folder);
    mkdir(str_folder);
    mkdir(apstr_folder);
    mkdir(mlstr_folder);
    mkdir(cum_str_folder);
    mkdir(cum_iso_folder);
    mkdir(cum_ani_folder);
    mkdir(angle_folder);
    mkdir(angmap_folder);
    
    crop = floor(spatial_filt_size/2)+1;
    xlims = [x(1,crop) x(1,end-crop)];
    ylims = [y(crop,1) y(end-crop,1)];
    
    roi = imread(path2roi);
    roi = roi(y(crop,2):y(end-crop,2), x(1,crop):x(1,end-crop));
    roi_perim = bwperim(roi);
    roi_perim = imdilate(roi_perim, strel('disk', 4));
    roi_props = regionprops(roi>0, 'centroid', 'area', 'PixelIdxList', 'BoundingBox');
    roi_centre = roi_props.Centroid;
    window_side = sqrt(roi_props.Area);

    bbox = round(roi_props.BoundingBox);
    [~, idx_a] = min(abs(x(1,:) - bbox(2)));
    [~, idx_b] = min(abs(x(1,:) - bbox(2) - bbox(4)));
    row_piv_inds = idx_a:idx_b; % indices of the roi rows in the velocity fields

    [~, idy_a] = min(abs(y(:,1) - bbox(1)));
    [~, idy_b] = min(abs(y(:,1) - bbox(1) - bbox(3)));
    col_piv_inds = idy_a:idy_b; % indices of the roi columns in the velocity fields


    % get positions so the roi ends up in the center of a window of
    % interest
    nx = (roi_centre(2)-window_side)/window_side;
    ny = (roi_centre(1)-window_side)/window_side;
    xo = round(mod(abs(nx),1)*window_side);
    yo = round(mod(abs(ny),1)*window_side);
    
    vel_thr=0.001;
    thickness=pi/16;
    ani_thr = 1e-4;

    data_grid = {};

    mag_values = {};
    iso_values = {};
    iso_x_values = {};
    iso_y_values = {};
    ani_values = {};
    str_values = {};
    apstr_values = {};
    mlstr_values = {};

    data_stripes = {};

   parfor t_id = 1:size(S,3)-1 % par
    
   % parfor t_id = 2:size(S,3)-1 % par
        
        I = S(:,:,t_id);
        n_windows = numel(xo:window_side:size(I, 2))*numel(yo:window_side:size(I, 1));
        data_grid{t_id}.vel = zeros(4,n_windows);
        data_grid{t_id}.vel_angle = zeros(4,n_windows);
        data_grid{t_id}.ani = zeros(4,n_windows);
        data_grid{t_id}.ani_angle = zeros(4,n_windows);
        data_grid{t_id}.iso = zeros(4,n_windows);
        data_grid{t_id}.iso_x = zeros(4,n_windows);
        data_grid{t_id}.iso_y = zeros(4,n_windows);
        data_grid{t_id}.str = zeros(4,n_windows);
        data_grid{t_id}.apstr = zeros(4,n_windows);
        data_grid{t_id}.mlstr = zeros(4,n_windows);

        data_stripes{t_id}.vel = zeros(size(u,1), length(col_piv_inds));
        data_stripes{t_id}.vel_angle = zeros(size(u,1), length(col_piv_inds));
        data_stripes{t_id}.ani = zeros(size(u,1), length(col_piv_inds));
        data_stripes{t_id}.ani_angle = zeros(size(u,1), length(col_piv_inds));
        data_stripes{t_id}.iso = zeros(size(u,1), length(col_piv_inds));
        data_stripes{t_id}.iso_x = zeros(size(u,1), length(col_piv_inds));
        data_stripes{t_id}.iso_y = zeros(size(u,1), length(col_piv_inds));
        data_stripes{t_id}.str = zeros(size(u,1), length(col_piv_inds));
        data_stripes{t_id}.apstr = zeros(size(u,1), length(col_piv_inds));
        data_stripes{t_id}.mlstr = zeros(size(u,1), length(col_piv_inds));

        clf
        
        M = ~logical(generate_mask(maskiererx, maskierery, t_id, xroi, yroi, I));
        M = M(y(crop,2):y(end-crop,2), x(1,crop):x(1,end-crop));
        M = imresize(M, [size(u,2), size(u,1)]);
    
        I = I(y(crop,2):y(end-crop,2), x(1,crop):x(1,end-crop));
        
        I = imadjust(I);
        I(roi_perim(:)) = 0;
        I2 = I;
        I2 = imadjust(I2);
        imshow(I2);
        
        hold on
        addScaleBar(pixel_s, 50);
    
    
        if t_id >= on_time_point && t_id <= off_time_point
            text(size(I,2)/5, size(I,1)/5, 'ON', 'FontSize', 30);
        else
            text(size(I,2)/5, size(I,1)/5, 'OFF', 'FontSize', 30);
        end
    
        export_fig([surf_folder filesep 'img_' num2str(t_id-1, '%04d') '.png']);
    
        %% activation

    
        %%
        clf
        
        I2 = I;
        alpha = 0.5;
        
        arrowSkip = 2;
        scale_factor = 1.5e3;
        
        vx = u(:,:,t_id).*M;
        vy = v(:,:,t_id).*M;
        mag = hypot(vx,vy);

        plot_angle_velocities(I, xo, yo, window_side, x, y, vx, vy, t_id, vel_thr, thickness, pixel_s, on_time_point, off_time_point, angle_folder);
        
        %collect angle
        vel_angles = atan2(vy, vx);
        vel_angles(mag < vel_thr) = nan;
        [mean_data, median_data, std_data, n_data] = gather_angle_data(I, vel_angles, xo, yo, window_side, x, y);
        data_grid{t_id}.vel_angle = [mean_data; median_data; std_data; n_data];
        
        % collect magnitude
        [mean_data, median_data, std_data, n_data] = gatherdata(I, mag, xo, yo, window_side, x, y);
        data_grid{t_id}.vel = [mean_data; median_data; std_data; n_data];

        data_stripes{t_id}.vel = mean(mag(:, col_piv_inds),2);

        clf
    
        imagesc(x(1,crop:end-crop),y(crop:end-crop,1), I2);
    
        set(gca,'xtick',[]);
        set(gca,'ytick',[]);
        axis image;
    
        set(gca, 'YDir','reverse'); set(gca, 'FontSize', 11);
        colormap('gray');
        freezeColors;
        
        mag_measure =  mag(crop:end-crop,crop:end-crop);
        roi_measure = imresize(double(roi), size(mag_measure))>0;
        mag_values{t_id} = mag_measure(roi_measure(:));
        
        hold on
        imagesc(y(crop:end-crop,1), x(1,crop:end-crop), mag(crop:end-crop,crop:end-crop),'AlphaData', alpha);
        
        hold on
        q = quiver(x(1,crop:arrowSkip:end-crop), y(crop:arrowSkip:end-crop,1),...
                  vx(crop:arrowSkip:end-crop,crop:arrowSkip:end-crop)*scale_factor,...
                  vy(crop:arrowSkip:end-crop,crop:arrowSkip:end-crop)*scale_factor);
    
        q.AutoScale = 'off';
    
        q.Color = 'w';
        q.LineWidth = 1.5;
        c = colorbar;
        c.Label.String = '\mu m/ s';
        caxis([0 0.02]);
    %     c.TickLabels = cellfun(@(x) num2str(str2double(x)*scale_factor), c.TickLabels, 'UniformOutput', false);
    
        set(gca,'xtick',[]);
        set(gca,'ytick',[]);
        axis image;
    
        set(gca, 'YDir','reverse'); 
    % set(gca, 'FontSize', 20);
        cmap = viridis;
        colormap(gca, cmap);
    
        xlim(xlims);
        ylim(ylims);
        title('Velocity field');
        addScaleBar(pixel_s, 50);
    
        if t_id >= on_time_point && t_id <= off_time_point
            text(size(I,2)/5, size(I,1)/5, 'ON', 'FontSize', 30);
        else
            text(size(I,2)/5, size(I,1)/5, 'OFF', 'FontSize', 30);
        end
    
        export_fig([vel_folder filesep 'img_' num2str(t_id-1, '%04d') '.png']);

        %% velocity angle continuous
        clf
        
        I2 = I;
        alpha = 0.5;
        
        arrowSkip = 1;
        scale_factor = 1.5e3/2;
        
        vx = u(:,:,t_id).*M;
        vy = v(:,:,t_id).*M;
        clf
    
        imagesc(x(1,crop:end-crop),y(crop:end-crop,1), I2);
    
        set(gca,'xtick',[]);
        set(gca,'ytick',[]);
        axis image;
    
        set(gca, 'YDir','reverse'); set(gca, 'FontSize', 11);
        colormap('gray');
        freezeColors;
        
%         mag_measure =  mag(crop:end-crop,crop:end-crop);
%         roi_measure = imresize(double(roi), size(mag_measure))>0;
%         mag_values{t_id} = mag_measure(roi_measure(:));
        
        vel_angles = atan2(vy, vx);
        vel_angles(~M) = nan;
%         vel_angles(mag < vel_thr) = nan;

        hold on
        imagesc(y(crop:end-crop,1), x(1,crop:end-crop), vel_angles(crop:end-crop,crop:end-crop),'AlphaData', alpha);
        
        hold on
        q = quiver(x(1,crop:arrowSkip:end-crop), y(crop:arrowSkip:end-crop,1),...
                  vx(crop:arrowSkip:end-crop,crop:arrowSkip:end-crop)*scale_factor,...
                  vy(crop:arrowSkip:end-crop,crop:arrowSkip:end-crop)*scale_factor);
    
        q.AutoScale = 'off';
    
        q.Color = 'w';
        q.LineWidth = 1.5/2;
        c = colorbar;
        c.Ticks = [-pi -pi/2 0 pi/2 pi];
        c.TickLabels = {'-\pi (left)', '-\pi/2 (up)','0 (right)','\pi/2 (down)','\pi (left)'};
        c.Label.String = '';
        caxis([-pi pi]);
    %     c.TickLabels = cellfun(@(x) num2str(str2double(x)*scale_factor), c.TickLabels, 'UniformOutput', false);
    
        set(gca,'xtick',[]);
        set(gca,'ytick',[]);
        axis image;
    
        set(gca, 'YDir','reverse'); 
    % set(gca, 'FontSize', 20);
%         cmap = [cmocean('phase')];
        cmap = [1, 1, 1; cmocean('phase')];
        colormap(gca, cmap);
    
        xlim(xlims);
        ylim(ylims);
        title('Angle');
        addScaleBar(pixel_s, 50);
    
        if t_id >= on_time_point && t_id <= off_time_point
            text(size(I,2)/5, size(I,1)/5, 'ON', 'FontSize', 30);
        else
            text(size(I,2)/5, size(I,1)/5, 'OFF', 'FontSize', 30);
        end
    
        export_fig([angmap_folder filesep 'img_' num2str(t_id-1, '%04d') '.png']);
    
        %% total strain


        clf
        
        I2 = I;
        I2 = imadjust(I2);
        alpha = 0.5;
        
        imagesc(x(1,crop:end-crop),y(crop:end-crop,1), I2);
    
        set(gca,'xtick',[]);
        set(gca,'ytick',[]);
        axis image;
    
        set(gca, 'YDir','reverse'); set(gca, 'FontSize', 11);
        colormap('gray');
        freezeColors;
    
        str = totStrain(:,:,t_id).*M;

        % collect str magnitude
        [mean_data, median_data, std_data, n_data] = gatherdata(I, str, xo, yo, window_side, x, y);
        data_grid{t_id}.str = [mean_data; median_data; std_data; n_data];

        data_stripes{t_id}.str = mean(str(:, col_piv_inds),2);

        str_measure =  str(crop:end-crop,crop:end-crop);
        roi_measure = imresize(double(roi), size(str_measure))>0;
        str_values{t_id} = str_measure(roi_measure(:));
        
        
        hold on
        imagesc(x(1,crop:end-crop), y(crop:end-crop,1), str(crop:end-crop,crop:end-crop),'AlphaData', alpha);
    
        colLim = 0.002;
        caxis([0 colLim]);
        c = colorbar;
        c.Label.String = '1 / s';
        c.Ticks = [ 0 colLim];
        c.TickLabels = num2str([0 colLim]');
    
        box on
    
        set(gca,'xtick',[]);
        set(gca,'ytick',[]);
        axis image;
        cmap = cmocean('amplitude');
%         cmap = viridis;
        colormap(gca, cmap);
    
        set(gca, 'YDir','reverse'); set(gca, 'FontSize', 11);
        addScaleBar(pixel_s, 50);
    
        title('Strain rate');
    
    
        if t_id >= on_time_point && t_id <= off_time_point
            text(size(I,2)/5, size(I,1)/5, 'ON', 'FontSize', 30);
        else
            text(size(I,2)/5, size(I,1)/5, 'OFF', 'FontSize', 30);
        end
        export_fig([str_folder filesep 'img_' num2str(t_id-1, '%04d') '.png']); 

         %% ap strain

        clf
        
        I2 = I;
        I2 = imadjust(I2);
        alpha = 0.5;
        
        imagesc(x(1,crop:end-crop),y(crop:end-crop,1), I2);
    
        set(gca,'xtick',[]);
        set(gca,'ytick',[]);
        axis image;
    
        set(gca, 'YDir','reverse'); set(gca, 'FontSize', 11);
        colormap('gray');
        freezeColors;
    
        str = apStrain(:,:,t_id).*M;

        % collect str magnitude
        [mean_data, median_data, std_data, n_data] = gatherdata(I, str, xo, yo, window_side, x, y);
        data_grid{t_id}.apstr = [mean_data; median_data; std_data; n_data];


        data_stripes{t_id}.apstr = mean(str(:, col_piv_inds),2);

        str_measure =  str(crop:end-crop,crop:end-crop);
        roi_measure = imresize(double(roi), size(str_measure))>0;
        apstr_values{t_id} = str_measure(roi_measure(:));
        
        
        hold on
        imagesc(x(1,crop:end-crop), y(crop:end-crop,1), str(crop:end-crop,crop:end-crop),'AlphaData', alpha);
    
        colLim = 0.002;
        caxis([0 colLim]);
        c = colorbar;
        c.Label.String = '1 / s';
        c.Ticks = [ 0 colLim];
        c.TickLabels = num2str([0 colLim]');
    
        box on
    
        set(gca,'xtick',[]);
        set(gca,'ytick',[]);
        axis image;
        cmap = cmocean('amplitude');
%         cmap = viridis;
        colormap(gca, cmap);
    
        set(gca, 'YDir','reverse'); set(gca, 'FontSize', 11);
        addScaleBar(pixel_s, 50);
    
        title('Strain rate');
    
    
        if t_id >= on_time_point && t_id <= off_time_point
            text(size(I,2)/5, size(I,1)/5, 'ON', 'FontSize', 30);
        else
            text(size(I,2)/5, size(I,1)/5, 'OFF', 'FontSize', 30);
        end
        export_fig([apstr_folder filesep 'img_' num2str(t_id-1, '%04d') '.png']);    


        %% ml strain


        clf
        
        I2 = I;
        I2 = imadjust(I2);
        alpha = 0.5;
        
        imagesc(x(1,crop:end-crop),y(crop:end-crop,1), I2);
    
        set(gca,'xtick',[]);
        set(gca,'ytick',[]);
        axis image;
    
        set(gca, 'YDir','reverse'); set(gca, 'FontSize', 11);
        colormap('gray');
        freezeColors;
    
        str = mlStrain(:,:,t_id).*M;

        % collect str magnitude
        [mean_data, median_data, std_data, n_data] = gatherdata(I, str, xo, yo, window_side, x, y);
        data_grid{t_id}.mlstr = [mean_data; median_data; std_data; n_data];


        data_stripes{t_id}.mlstr = mean(str(:, col_piv_inds),2);

        str_measure =  str(crop:end-crop,crop:end-crop);
        roi_measure = imresize(double(roi), size(str_measure))>0;
        mlstr_values{t_id} = str_measure(roi_measure(:));
        
        
        hold on
        imagesc(x(1,crop:end-crop), y(crop:end-crop,1), str(crop:end-crop,crop:end-crop),'AlphaData', alpha);
    
        colLim = 0.002;
        caxis([0 colLim]);
        c = colorbar;
        c.Label.String = '1 / s';
        c.Ticks = [ 0 colLim];
        c.TickLabels = num2str([0 colLim]');
    
        box on
    
        set(gca,'xtick',[]);
        set(gca,'ytick',[]);
        axis image;
        cmap = cmocean('amplitude');
%         cmap = viridis;
        colormap(gca, cmap);
    
        set(gca, 'YDir','reverse'); set(gca, 'FontSize', 11);
        addScaleBar(pixel_s, 50);
    
        title('Strain rate');
    
    
        if t_id >= on_time_point && t_id <= off_time_point
            text(size(I,2)/5, size(I,1)/5, 'ON', 'FontSize', 30);
        else
            text(size(I,2)/5, size(I,1)/5, 'OFF', 'FontSize', 30);
        end
        export_fig([mlstr_folder filesep 'img_' num2str(t_id-1, '%04d') '.png']);    


        %% cumulative total strain


        clf
        
        I2 = I;
        I2 = imadjust(I2);
        alpha = 0.5;
        
        imagesc(x(1,crop:end-crop),y(crop:end-crop,1), I2);
    
        set(gca,'xtick',[]);
        set(gca,'ytick',[]);
        axis image;
    
        set(gca, 'YDir','reverse'); set(gca, 'FontSize', 11);
        colormap('gray');
        freezeColors;
    
        str = sum(totStrain(:,:,1:t_id),3).*M;

        % collect str magnitude
        [mean_data, median_data, std_data, n_data] = gatherdata(I, str, xo, yo, window_side, x, y);
        data_grid{t_id}.str = [mean_data; median_data; std_data; n_data];

        str_measure =  str(crop:end-crop,crop:end-crop);
        roi_measure = imresize(double(roi), size(str_measure))>0;
        str_values{t_id} = str_measure(roi_measure(:));
        
        
        hold on
        imagesc(x(1,crop:end-crop), y(crop:end-crop,1), str(crop:end-crop,crop:end-crop),'AlphaData', alpha);
    
        colLim = 0.1;%max(str(:));
        caxis([0 colLim]);
        c = colorbar;
        c.Label.String = '1 / s';
        c.Ticks = [ 0 colLim];
        c.TickLabels = num2str([0 colLim]');
    
        box on
    
        set(gca,'xtick',[]);
        set(gca,'ytick',[]);
        axis image;
        cmap = cmocean('amplitude');
%         cmap = viridis;
        colormap(gca, cmap);
    
        set(gca, 'YDir','reverse'); set(gca, 'FontSize', 11);
        addScaleBar(pixel_s, 50);
    
        title('cumulative strain');
    
    
        if t_id >= on_time_point && t_id <= off_time_point
            text(size(I,2)/5, size(I,1)/5, 'ON', 'FontSize', 30);
        else
            text(size(I,2)/5, size(I,1)/5, 'OFF', 'FontSize', 30);
        end
        export_fig([cum_str_folder filesep 'img_' num2str(t_id-1, '%04d') '.png']);    


        %%
    
        clf
        
        I2 = I;
        I2 = imadjust(I2);
        alpha = 0.5;
        
        imagesc(x(1,crop:end-crop),y(crop:end-crop,1), I2);
    
        set(gca,'xtick',[]);
        set(gca,'ytick',[]);
        axis image;
    
        set(gca, 'YDir','reverse'); set(gca, 'FontSize', 11);
        colormap('gray');
        freezeColors;
    
        iso = isoStrain(:,:,t_id).*M;

        % collect iso magnitude
        [mean_data, median_data, std_data, n_data] = gatherdata(I, iso, xo, yo, window_side, x, y);
        data_grid{t_id}.iso = [mean_data; median_data; std_data; n_data];

        data_stripes{t_id}.iso = mean(iso(:, col_piv_inds),2);

        iso_measure =  iso(crop:end-crop,crop:end-crop);
        roi_measure = imresize(double(roi), size(iso_measure))>0;
        iso_values{t_id} = iso_measure(roi_measure(:));
        
        
        hold on
        imagesc(x(1,crop:end-crop), y(crop:end-crop,1), iso(crop:end-crop,crop:end-crop),'AlphaData', alpha);
    
        colLim = 0.001;
        caxis([-colLim colLim]);
        c = colorbar;
        c.Label.String = '1 / s';
        c.Ticks = [-colLim 0 colLim];
        c.TickLabels = num2str([-colLim 0 colLim]'); 
    
        box on
    
        set(gca,'xtick',[]);
        set(gca,'ytick',[]);
        axis image;
        cmap = cmocean('balance');
        colormap(gca, cmap);
    
        set(gca, 'YDir','reverse'); set(gca, 'FontSize', 11);
        addScaleBar(pixel_s, 50);
    
        title('Isotropic part (strain rate)');
    
    
        if t_id >= on_time_point && t_id <= off_time_point
            text(size(I,2)/5, size(I,1)/5, 'ON', 'FontSize', 30);
        else
            text(size(I,2)/5, size(I,1)/5, 'OFF', 'FontSize', 30);
        end
        export_fig([iso_folder filesep 'img_' num2str(t_id-1, '%04d') '.png']);    

         %%
    
        clf
        
        I2 = I;
        I2 = imadjust(I2);
        alpha = 0.5;
        
        imagesc(x(1,crop:end-crop),y(crop:end-crop,1), I2);
    
        set(gca,'xtick',[]);
        set(gca,'ytick',[]);
        axis image;
    
        set(gca, 'YDir','reverse'); set(gca, 'FontSize', 11);
        colormap('gray');
        freezeColors;
    
        iso_x = isoStrainx(:,:,t_id).*M;

        % collect iso magnitude
        [mean_data, median_data, std_data, n_data] = gatherdata(I, iso_x, xo, yo, window_side, x, y);
        data_grid{t_id}.iso_x = [mean_data; median_data; std_data; n_data];

        data_stripes{t_id}.iso_x = mean(iso_x(:, col_piv_inds),2);

        iso_x_measure =  iso_x(crop:end-crop,crop:end-crop);
        roi_measure = imresize(double(roi), size(iso_x_measure))>0;
        iso_x_values{t_id} = iso_x_measure(roi_measure(:));
        
        
        hold on
        imagesc(x(1,crop:end-crop), y(crop:end-crop,1), iso_x(crop:end-crop,crop:end-crop),'AlphaData', alpha);
    
        colLim = 0.001;
        caxis([-colLim colLim]);
        c = colorbar;
        c.Label.String = '1 / s';
        c.Ticks = [-colLim 0 colLim];
        c.TickLabels = num2str([-colLim 0 colLim]'); 
    
        box on
    
        set(gca,'xtick',[]);
        set(gca,'ytick',[]);
        axis image;
        cmap = cmocean('balance');
        colormap(gca, cmap);
    
        set(gca, 'YDir','reverse'); set(gca, 'FontSize', 11);
        addScaleBar(pixel_s, 50);
    
        title('Isotropic X part (strain rate)');
    
    
        if t_id >= on_time_point && t_id <= off_time_point
            text(size(I,2)/5, size(I,1)/5, 'ON', 'FontSize', 30);
        else
            text(size(I,2)/5, size(I,1)/5, 'OFF', 'FontSize', 30);
        end
        export_fig([iso_x_folder filesep 'img_' num2str(t_id-1, '%04d') '.png']);    

         %%
    
        clf
        
        I2 = I;
        I2 = imadjust(I2);
        alpha = 0.5;
        
        imagesc(x(1,crop:end-crop),y(crop:end-crop,1), I2);
    
        set(gca,'xtick',[]);
        set(gca,'ytick',[]);
        axis image;
    
        set(gca, 'YDir','reverse'); set(gca, 'FontSize', 11);
        colormap('gray');
        freezeColors;
    
        iso_y = isoStrainy(:,:,t_id).*M;

        % collect iso y magnitude
        [mean_data, median_data, std_data, n_data] = gatherdata(I, iso_y, xo, yo, window_side, x, y);
        data_grid{t_id}.iso_y = [mean_data; median_data; std_data; n_data];

        data_stripes{t_id}.iso_y = mean(iso_y(:, col_piv_inds),2);

        iso_y_measure =  iso_y(crop:end-crop,crop:end-crop);
        roi_measure = imresize(double(roi), size(iso_y_measure))>0;
        iso_y_values{t_id} = iso_y_measure(roi_measure(:));
        
        
        hold on
        imagesc(x(1,crop:end-crop), y(crop:end-crop,1), iso_y(crop:end-crop,crop:end-crop),'AlphaData', alpha);
    
        colLim = 0.001;
        caxis([-colLim colLim]);
        c = colorbar;
        c.Label.String = '1 / s';
        c.Ticks = [-colLim 0 colLim];
        c.TickLabels = num2str([-colLim 0 colLim]'); 
    
        box on
    
        set(gca,'xtick',[]);
        set(gca,'ytick',[]);
        axis image;
        cmap = cmocean('balance');
        colormap(gca, cmap);
    
        set(gca, 'YDir','reverse'); set(gca, 'FontSize', 11);
        addScaleBar(pixel_s, 50);
    
        title('Isotropic Y part (strain rate)');
    
    
        if t_id >= on_time_point && t_id <= off_time_point
            text(size(I,2)/5, size(I,1)/5, 'ON', 'FontSize', 30);
        else
            text(size(I,2)/5, size(I,1)/5, 'OFF', 'FontSize', 30);
        end
        export_fig([iso_y_folder filesep 'img_' num2str(t_id-1, '%04d') '.png']);    
    
        
       %% cumulative iso
    
        clf
        
        I2 = I;
        I2 = imadjust(I2);
        alpha = 0.5;
        
        imagesc(x(1,crop:end-crop),y(crop:end-crop,1), I2);
    
        set(gca,'xtick',[]);
        set(gca,'ytick',[]);
        axis image;
    
        set(gca, 'YDir','reverse'); set(gca, 'FontSize', 11);
        colormap('gray');
        freezeColors;
    
        iso = sum(isoStrain(:,:,1:t_id).*M, 3);
        
        
        hold on
        imagesc(x(1,crop:end-crop), y(crop:end-crop,1), iso(crop:end-crop,crop:end-crop),'AlphaData', alpha);
    
        colLim = 0.025;
        caxis([-colLim colLim]);
        c = colorbar;
        c.Label.String = '1 / s';
        c.Ticks = [-colLim 0 colLim];
        c.TickLabels = num2str([-colLim 0 colLim]'); 
    
        box on
    
        set(gca,'xtick',[]);
        set(gca,'ytick',[]);
        axis image;
        cmap = cmocean('balance');
        colormap(gca, cmap);
    
        set(gca, 'YDir','reverse'); set(gca, 'FontSize', 11);
        addScaleBar(pixel_s, 50);
    
        title('Isotropic part (strain rate)');
    
    
        if t_id >= on_time_point && t_id <= off_time_point
            text(size(I,2)/5, size(I,1)/5, 'ON', 'FontSize', 30);
        else
            text(size(I,2)/5, size(I,1)/5, 'OFF', 'FontSize', 30);
        end
        export_fig([cum_iso_folder filesep 'img_' num2str(t_id-1, '%04d') '.png']);    
    
 %%    
        clf
        I2 = I;
        I2 = imadjust(I2);
        alpha = 0.25;
        
        imagesc(x(1,crop:end-crop),y(crop:end-crop,1), I2);
    
        set(gca,'xtick',[]);
        set(gca,'ytick',[]);
        axis image;
    
        set(gca, 'YDir','reverse'); set(gca, 'FontSize', 11);
        colormap('gray');
        freezeColors;
        
        arrowSkip = 2;
    
        sk = 1;
        kernel = ones(sk)./sk^2;
    
        ani = aniStrain(:,:,t_id).*M;
        
        % collect ani magnitude
        [mean_data, median_data, std_data, n_data] = gatherdata(I, ani, xo, yo, window_side, x, y);
        data_grid{t_id}.ani = [mean_data; median_data; std_data; n_data];

        data_stripes{t_id}.ani = mean(ani(:, col_piv_inds),2);

        ani_measure =  ani(crop:end-crop,crop:end-crop);
        roi_measure = imresize(double(roi), size(ani_measure))>0;
        ani_values{t_id} = ani_measure(roi_measure(:));
        
        dirs = aniDirs(:,:,:,t_id);
        dirs(:,:,1) = imfilter(dirs(:,:,1), kernel);
        dirs(:,:,2) = imfilter(dirs(:,:,2), kernel);
        
        % collect ani angle (check)
        dirs_angles = atan2(dirs(:,:,2), dirs(:,:,1));
        dirs_angles(ani < ani_thr) = nan;
        [mean_data, median_data, std_data, n_data] = gather_angle_data(I, dirs_angles, xo, yo, window_side, x, y);
        data_grid{t_id}.ani_angle = [mean_data; median_data; std_data; n_data];

        hold on
        imagesc(x(1,crop:end-crop), y(crop:end-crop,1), ani(crop:end-crop,crop:end-crop),'AlphaData', alpha);
        colLim = 0.001;
        c = colorbar;
        c.Ticks = [0 colLim/2 colLim];
        c.TickLabels = num2str([0 colLim/2 colLim]'); 
        c.Label.String = '1 / h';
        caxis([0 colLim]);
        
        cmap = cmocean('amp');
        colormap(gca, cmap);
        
        
        dirs = dirs(crop:arrowSkip:end-crop,crop:arrowSkip:end-crop,:);
        correction = 4e4*ani(crop:arrowSkip:end-crop,crop:arrowSkip:end-crop);
        q = quiver(x(crop:arrowSkip:end-crop,crop:arrowSkip:end-crop)-0.5*dirs(:,:,1).*correction,y(crop:arrowSkip:end-crop,crop:arrowSkip:end-crop)-0.5*dirs(:,:,2).*correction,...
            correction.*dirs(:,:,1),correction.*dirs(:,:,2));
        
        q.AutoScale = 'off';
        q.Color = 'k';
        q.ShowArrowHead = 'off';
        q.LineWidth = 1.5;
        
        box on
    
        set(gca,'xtick',[]);
        set(gca,'ytick',[]);
        axis image;
    
        set(gca, 'YDir','reverse'); set(gca, 'FontSize', 11);
    
        title('Anisotropic part (strain rate)');
        addScaleBar(pixel_s, 50);
    
        if t_id >= on_time_point && t_id <= off_time_point
            text(size(I,2)/5, size(I,1)/5, 'ON', 'FontSize', 30);
        else
            text(size(I,2)/5, size(I,1)/5, 'OFF', 'FontSize', 30);
        end
        
        export_fig([ani_folder filesep 'img_' num2str(t_id-1, '%04d') '.png']);

        %% cumulative ani strain    
        clf
        I2 = I;
        I2 = imadjust(I2);
        alpha = 0.5;
        
        imagesc(x(1,crop:end-crop),y(crop:end-crop,1), I2);
    
        set(gca,'xtick',[]);
        set(gca,'ytick',[]);
        axis image;
    
        set(gca, 'YDir','reverse'); set(gca, 'FontSize', 11);
        colormap('gray');
        freezeColors;
        
        arrowSkip = 2;
    
        sk = 1;
        kernel = ones(sk)./sk^2;
    
        ani = sum(aniStrain(:,:,1:t_id).*M,3);
       
        
        dirs = aniDirs(:,:,:,t_id);
        dirs(:,:,1) = imfilter(dirs(:,:,1), kernel);
        dirs(:,:,2) = imfilter(dirs(:,:,2), kernel);
        
        hold on
        imagesc(x(1,crop:end-crop), y(crop:end-crop,1), ani(crop:end-crop,crop:end-crop),'AlphaData', alpha);
        colLim = 0.025;
        c = colorbar;
        c.Ticks = [0 colLim/2 colLim];
        c.TickLabels = num2str([0 colLim/2 colLim]'); 
        c.Label.String = '1 / h';
        caxis([0 colLim]);
        
        cmap = cmocean('amp');
        colormap(gca, cmap);
        
        
%         dirs = dirs(crop:arrowSkip:end-crop,crop:arrowSkip:end-crop,:);
%         correction = 1;
%         q = quiver(x(crop:arrowSkip:end-crop,crop:arrowSkip:end-crop)-0.5*dirs(:,:,1).*correction,y(crop:arrowSkip:end-crop,crop:arrowSkip:end-crop)-0.5*dirs(:,:,2).*correction,...
%             correction.*dirs(:,:,1),correction.*dirs(:,:,2));
        
%         q.AutoScale = 'off';
%         q.Color = 'k';
%         q.ShowArrowHead = 'off';
%         q.LineWidth = 1.5;
%         
        box on
    
        set(gca,'xtick',[]);
        set(gca,'ytick',[]);
        axis image;
    
        set(gca, 'YDir','reverse'); set(gca, 'FontSize', 11);
    
        title('Anisotropic part (strain rate)');
        addScaleBar(pixel_s, 50);
    
        if t_id >= on_time_point && t_id <= off_time_point
            text(size(I,2)/5, size(I,1)/5, 'ON', 'FontSize', 30);
        else
            text(size(I,2)/5, size(I,1)/5, 'OFF', 'FontSize', 30);
        end
        
        export_fig([cum_ani_folder filesep 'img_' num2str(t_id-1, '%04d') '.png']);

    end
    
    save([out_folder filesep 'grid_data'], 'data_grid');
    save([out_folder filesep 'data_stripes'], 'data_stripes');
    
    eR = expReader(surf_folder);
    %%eR.ffmpeg;
    
    eR = expReader(vel_folder);
    %%eR.ffmpeg;
    
    eR = expReader(iso_folder);
    %%eR.ffmpeg;
    
    eR = expReader(ani_folder);
    %%eR.ffmpeg;
    
    disp('Done!')

       %%

    fields = {'vel', 'str', 'apstr', 'mlstr', 'ani', 'iso', 'iso_x', 'iso_y'};

    units = {'\mum/s', '1/s', '1/s', '1/s', '1/s', '1/s', '1/s', '1/s',};
    
    for field_idx = 1:length(fields)
        
        field = fields{field_idx};
        unit = units{field_idx};
        clf
        for t_id = 2:size(u,3)-1
            period = 'Before'; color = 'k'; plot_idx = 1;
            if t_id > on_time_point; period = 'Pulse'; plot_idx = 2; color = 'r'; end
            if t_id > off_time_point; period = 'After'; plot_idx = 3; color = 'b'; end
            hold on
            subplot(3, 1, plot_idx);
            title(period);
            plot(data_stripes{t_id}.(field)(crop:end-crop), 'Color',  color);
        
        %     ylim([0, 0.02]);
        end
        
        min_val = inf;
        max_val = -inf;
        for plot_idx = 1:3
            subplot(3, 1, plot_idx);
        
            min_val = min(min_val, min(gca().YLim));
            max_val = max(max_val, max(gca().YLim));
        end
        
        for plot_idx = 1:3
            subplot(3, 1, plot_idx);
        
            ylim([min_val, max_val]);
            xlim([1, length(data_stripes{t_id}.iso(crop:end-crop))]);
            patch([row_piv_inds(1), row_piv_inds(end), row_piv_inds(end), row_piv_inds(1)], [min_val, min_val, max_val, max_val], [0.9, 0.9, 0.9], 'FaceAlpha',0.3);
            ylabel(unit); box on; grid on
            xticks([]);
    
        end
        
        set(gcf, 'Color', 'w');
        xlabel('N \rightarrow S');
        export_fig([out_folder filesep 'north_south_column_analysis_' field '.png']);
        
        data_matrix = cell2mat(arrayfun(@(t) data_stripes{t}.(field)(crop:end-crop), 2:length(data_stripes), 'UniformOutput', false));

        timepoints = 2:size(data_matrix,2)+1;
        T = array2table(data_matrix);
        T.Properties.VariableNames = sprintfc('t%d', timepoints)';
        T.ROI = double(ismember([1:length(data_stripes{t_id}.(field)(crop:end-crop))], row_piv_inds)');
        T.POSITION = y(crop:end-crop,1)*pixel_s;

        currentDate = datestr(now, 'yyyy_mmm_dd');
        xlsx_folder = [experiment_folder filesep 'xlsx_data' filesep currentDate ]; 
        if not(isfolder(xlsx_folder)) %~exist(xlsx_folder, 'dir')
            mkdir(xlsx_folder); 
        end 

        writetable(T, [xlsx_folder filesep expdate '_' expname 'column_analysis_data_' field '.xlsx']);
    end
    %%
%     max_val = 0.025;
%     min_val = 0;
%     
%     
%     I = S(:,:,1);
%     
%     vel_avg_global = [];
%     vel_std_global = [];
%     mag_vel = hypot(u, v);
%     for t_id = 1:size(mag_vel,3)
%     
%         M = ~logical(generate_mask(maskiererx, maskierery, t_id, xroi, yroi, I));
%         M = M(y(crop,2):y(end-crop,2), x(1,crop):x(1,end-crop));
%         M = imresize(M, [size(mag_vel,1), size(mag_vel,2)]);
%         roi_measure = imresize(~(roi), [size(mag_vel,1), size(mag_vel,2)])>0;
%         
%         mag_vel(:,:,t_id) = mag_vel(:,:,t_id).*M.*roi_measure;
%     
%         vel_avg_global(t_id) = mean(extend(mag_vel(crop:end-crop,crop:end-crop,t_id)));
%         vel_std_global(t_id) = std(extend(mag_vel(crop:end-crop,crop:end-crop,t_id)));
%     end
%     
%     clf
%     hold on
%     
%     patch([on_time_point, off_time_point, off_time_point, on_time_point], [min_val, min_val, max_val, max_val], [0.9, 0.9, 0.9], 'FaceAlpha',0.3);
%     errorbar(cellfun(@(x) median(x), mag_values), cellfun(@(x) std(x), mag_values));
%     errorbar(vel_avg_global, vel_std_global);
%     
%     title('Speed');
%     legend('exposure', 'ROI', 'whole')
%     box on; grid on
%     ylim([min_val, max_val]);
% 
%    
%     
%     export_fig([out_folder filesep expdate '_' expname '_roi_velocity.png']);
%     %%
%     max_val = 8e-4;
%     min_val = -8e-4;
%     
%     iso_avg_global = [];
%     iso_std_global = [];
%     isoStrain_mag = isoStrain;
%     for t_id = 1:size(isoStrain,3)
%     
%         M = ~logical(generate_mask(maskiererx, maskierery, t_id, xroi, yroi, I));
%         M = M(y(crop,2):y(end-crop,2), x(1,crop):x(1,end-crop));
%         M = imresize(M, [size(isoStrain_mag,1), size(isoStrain_mag,2)]);
%         roi_measure = imresize(~(roi), [size(isoStrain_mag,1), size(isoStrain_mag,2)])>0;
%         
%         isoStrain_mag(:,:,t_id) = isoStrain_mag(:,:,t_id).*M.*roi_measure;
%     
%         iso_avg_global(t_id) = mean(extend(isoStrain_mag(crop:end-crop,crop:end-crop,t_id)));
%         iso_std_global(t_id) = std(extend(isoStrain_mag(crop:end-crop,crop:end-crop,t_id)));
%     end
%     
%     clf
%     hold on
%     
%     patch([on_time_point, off_time_point, off_time_point, on_time_point], [min_val, min_val, max_val, max_val], [0.9, 0.9, 0.9], 'FaceAlpha',0.3);
%     errorbar(cellfun(@(x) median(x), iso_values), cellfun(@(x) std(x), iso_values));
%     errorbar(iso_avg_global, iso_std_global);
%     
%     title('Isotropic Part');
%     legend('exposure', 'ROI', 'whole');
%     box on; grid on
%     ylim([min_val, max_val]);
%     export_fig([out_folder filesep expdate '_' expname  '_roi_iso.png']);
%     %%
%     ani_avg_global = [];
%     ani_std_global = [];
%     aniStrain_mag = aniStrain;
%     for t_id = 1:size(aniStrain,3)
%     
%         M = ~logical(generate_mask(maskiererx, maskierery, t_id, xroi, yroi, I));
%         M = M(y(crop,2):y(end-crop,2), x(1,crop):x(1,end-crop));
%         M = imresize(M, [size(aniStrain_mag,1), size(aniStrain_mag,2)]);
%         roi_measure = imresize(~(roi), [size(aniStrain_mag,1), size(aniStrain_mag,2)])>0;
%         
%         aniStrain_mag(:,:,t_id) = aniStrain_mag(:,:,t_id).*M.*roi_measure;
%     
%         ani_avg_global(t_id) = mean(extend(aniStrain(crop:end-crop,crop:end-crop,t_id)));
%         ani_std_global(t_id) = std(extend(aniStrain(crop:end-crop,crop:end-crop,t_id)));
%     end
%     
%     clf
%     hold on
%     
%     patch([on_time_point, off_time_point, off_time_point, on_time_point], [min_val, min_val, max_val, max_val], [0.9, 0.9, 0.9], 'FaceAlpha',0.3);
%     errorbar(cellfun(@(x) median(x), ani_values), cellfun(@(x) std(x), ani_values));
%     errorbar(ani_avg_global, ani_std_global);
%     
%     title('Anisotropic Part');
%     legend('exposure', 'ROI', 'whole');
%     box on; grid on
%     ylim([min_val, max_val]);
%     export_fig([out_folder filesep expdate '_' expname  '_roi_ani.png']);
%     
%%
%{
Not my most elegant code. Plot the grid NWSE data
%}

I = S(:,:,1);
windows_x = [xo:window_side:size(I, 2)-window_side];
windows_y = [yo:window_side:size(I, 1)-window_side];
Nc = [round(roi_centre(1)), round(roi_centre(2))-window_side];
Sc = [round(roi_centre(1)), round(roi_centre(2))+window_side];
Wc = [round(roi_centre(1))-window_side, round(roi_centre(2))];
Ec = [round(roi_centre(1))+window_side, round(roi_centre(2))];

coords = [];
coords_idx = [];
counter = 1;
for i = windows_x
    for j = windows_y
        coords(end+1, 1:2) = [j, i];
        coords_idx(end+1) = counter;
        counter = counter+1;
    end
end

[~, Ni] = min(pdist2(Nc, coords));
[~, Si] = min(pdist2(Sc, coords));
[~, Wi] = min(pdist2(Wc, coords));
[~, Ei] = min(pdist2(Ec, coords));
[~, Ci] = min(pdist2(roi_centre, coords));
%% Velocity magnitude
clf
% North
max_val = 0.025;
min_val = -0.001;
vel_mean = [];
vel_std = [];
for t_id = 2:length(data_grid)
    vel_mean(t_id) = data_grid{t_id}.vel(1,Ni); %[mean_data, median_data, std_data, n_data]
    vel_std(t_id) = data_grid{t_id}.vel(3,Ni); %[mean_data, median_data, std_data, n_data]
end

subplot(3,3,2);
title('Velocity');
hold on
patch([on_time_point, off_time_point, off_time_point, on_time_point], [min_val, min_val, max_val, max_val], [0.9, 0.9, 0.9], 'FaceAlpha',0.3);
errorbar(vel_mean, vel_std);

%legend('exposure', 'ROI', 'whole');

box on; grid on
ylim([min_val, max_val]);

% West
vel_mean = [];
vel_std = [];
for t_id = 2:length(data_grid)
    vel_mean(t_id) = data_grid{t_id}.vel(1,Wi); %[mean_data, median_data, std_data, n_data]
    vel_std(t_id) = data_grid{t_id}.vel(3,Wi); %[mean_data, median_data, std_data, n_data]
end

subplot(3,3,4);
hold on
patch([on_time_point, off_time_point, off_time_point, on_time_point], [min_val, min_val, max_val, max_val], [0.9, 0.9, 0.9], 'FaceAlpha',0.3);
errorbar(vel_mean, vel_std);

%legend('exposure', 'ROI', 'whole');
box on; grid on
ylim([min_val, max_val]);

% Centre
vel_mean = [];
vel_std = [];
for t_id = 2:length(data_grid)
    vel_mean(t_id) = data_grid{t_id}.vel(1,Ci); %[mean_data, median_data, std_data, n_data]
    vel_std(t_id) = data_grid{t_id}.vel(3,Ci); %[mean_data, median_data, std_data, n_data]
end

subplot(3,3,5);
hold on
patch([on_time_point, off_time_point, off_time_point, on_time_point], [min_val, min_val, max_val, max_val], [0.9, 0.9, 0.9], 'FaceAlpha',0.3);
errorbar(vel_mean, vel_std);

%legend('exposure', 'ROI', 'whole');
box on; grid on
ylim([min_val, max_val]);

% East
vel_mean = [];
vel_std = [];
for t_id = 2:length(data_grid)
    vel_mean(t_id) = data_grid{t_id}.vel(1,Ei); %[mean_data, median_data, std_data, n_data]
    vel_std(t_id) = data_grid{t_id}.vel(3,Ei); %[mean_data, median_data, std_data, n_data]
end

subplot(3,3,6);
hold on
patch([on_time_point, off_time_point, off_time_point, on_time_point], [min_val, min_val, max_val, max_val], [0.9, 0.9, 0.9], 'FaceAlpha',0.3);
errorbar(vel_mean, vel_std);

%legend('exposure', 'ROI', 'whole');
box on; grid on
ylim([min_val, max_val]);

% South
vel_mean = [];
vel_std = [];
for t_id = 2:length(data_grid)
    vel_mean(t_id) = data_grid{t_id}.vel(1,Si); %[mean_data, median_data, std_data, n_data]
    vel_std(t_id) = data_grid{t_id}.vel(3,Si); %[mean_data, median_data, std_data, n_data]
end

subplot(3,3,8);
hold on
patch([on_time_point, off_time_point, off_time_point, on_time_point], [min_val, min_val, max_val, max_val], [0.9, 0.9, 0.9], 'FaceAlpha',0.3);
errorbar(vel_mean, vel_std);

%legend('exposure', 'ROI', 'whole');
box on; grid on
ylim([min_val, max_val]);
export_fig([out_folder filesep expdate '_' expname  '_NSWE_vel.png']);

%% Velocity angle
clf
% North
max_val = pi;
min_val = -pi;
vel_mean = [];
vel_std = [];
for t_id = 2:length(data_grid)
    vel_mean(t_id) = data_grid{t_id}.vel_angle(1,Ni); %[mean_data, median_data, std_data, n_data]
    vel_std(t_id) = data_grid{t_id}.vel_angle(3,Ni); %[mean_data, median_data, std_data, n_data]
end

subplot(3,3,2);
title('Velocity angle');
hold on
patch([on_time_point, off_time_point, off_time_point, on_time_point], [min_val, min_val, max_val, max_val], [0.9, 0.9, 0.9], 'FaceAlpha',0.3);
errorbar(vel_mean, vel_std);
set(gca,'YTick',-pi:pi/2:pi) 
set(gca,'YTickLabel',{'-\pi (left)', '-\pi/2 (up)','0 (right)','\pi/2 (down)','\pi (left)'});
%legend('exposure', 'ROI', 'whole');

box on; grid on
ylim([min_val, max_val]);

% West
vel_mean = [];
vel_std = [];
for t_id = 2:length(data_grid)
    vel_mean(t_id) = data_grid{t_id}.vel_angle(1,Wi); %[mean_data, median_data, std_data, n_data]
    vel_std(t_id) = data_grid{t_id}.vel_angle(3,Wi); %[mean_data, median_data, std_data, n_data]
end

subplot(3,3,4);
hold on
patch([on_time_point, off_time_point, off_time_point, on_time_point], [min_val, min_val, max_val, max_val], [0.9, 0.9, 0.9], 'FaceAlpha',0.3);
errorbar(vel_mean, vel_std);
set(gca,'YTick',-pi:pi/2:pi); 
set(gca,'YTickLabel',{'-\pi (left)', '-\pi/2 (up)','0 (right)','\pi/2 (down)','\pi (left)'});
%legend('exposure', 'ROI', 'whole');
box on; grid on
ylim([min_val, max_val]);

% Centre
vel_mean = [];
vel_std = [];
for t_id = 2:length(data_grid)
    vel_mean(t_id) = data_grid{t_id}.vel_angle(1,Ci); %[mean_data, median_data, std_data, n_data]
    vel_std(t_id) = data_grid{t_id}.vel_angle(3,Ci); %[mean_data, median_data, std_data, n_data]
end

subplot(3,3,5);
hold on
patch([on_time_point, off_time_point, off_time_point, on_time_point], [min_val, min_val, max_val, max_val], [0.9, 0.9, 0.9], 'FaceAlpha',0.3);
errorbar(vel_mean, vel_std);
set(gca,'YTick',-pi:pi/2:pi); 
set(gca,'YTickLabel',{'-\pi (left)', '-\pi/2 (up)','0 (right)','\pi/2 (down)','\pi (left)'});
%legend('exposure', 'ROI', 'whole');
box on; grid on
ylim([min_val, max_val]);

% East
vel_mean = [];
vel_std = [];
for t_id = 2:length(data_grid)
    vel_mean(t_id) = data_grid{t_id}.vel_angle(1,Ei); %[mean_data, median_data, std_data, n_data]
    vel_std(t_id) = data_grid{t_id}.vel_angle(3,Ei); %[mean_data, median_data, std_data, n_data]
end

subplot(3,3,6);
hold on
patch([on_time_point, off_time_point, off_time_point, on_time_point], [min_val, min_val, max_val, max_val], [0.9, 0.9, 0.9], 'FaceAlpha',0.3);
errorbar(vel_mean, vel_std);
set(gca,'YTick',-pi:pi/2:pi);
set(gca,'YTickLabel',{'-\pi (left)', '-\pi/2 (up)','0 (right)','\pi/2 (down)','\pi (left)'});
%legend('exposure', 'ROI', 'whole');
box on; grid on
ylim([min_val, max_val]);

% South
vel_mean = [];
vel_std = [];
for t_id = 2:length(data_grid)
    vel_mean(t_id) = data_grid{t_id}.vel_angle(1,Si); %[mean_data, median_data, std_data, n_data]
    vel_std(t_id) = data_grid{t_id}.vel_angle(3,Si); %[mean_data, median_data, std_data, n_data]
end

subplot(3,3,8);
hold on
patch([on_time_point, off_time_point, off_time_point, on_time_point], [min_val, min_val, max_val, max_val], [0.9, 0.9, 0.9], 'FaceAlpha',0.3);
errorbar(vel_mean, vel_std);
set(gca,'YTick',-pi:pi/2:pi);
set(gca,'YTickLabel',{'-\pi (left)', '-\pi/2 (up)','0 (right)','\pi/2 (down)','\pi (left)'});
%legend('exposure', 'ROI', 'whole');
box on; grid on
ylim([min_val, max_val]);
export_fig([out_folder filesep expdate '_' expname  '_NSWE_vel_angle.png']);

%% str
clf
% North
max_val = 18e-4;
min_val = 0e-4;

vel_mean = [];
vel_std = [];
for t_id = 2:length(data_grid)
    vel_mean(t_id) = data_grid{t_id}.str(1,Ni); %[mean_data, median_data, std_data, n_data]
    vel_std(t_id) = data_grid{t_id}.str(3,Ni); %[mean_data, median_data, std_data, n_data]
end

subplot(3,3,2);
title('Strain rate');
hold on
patch([on_time_point, off_time_point, off_time_point, on_time_point], [min_val, min_val, max_val, max_val], [0.9, 0.9, 0.9], 'FaceAlpha',0.3);
errorbar(vel_mean, vel_std);
yline(0, 'LineWidth', 2);
%legend('exposure', 'ROI', 'whole');

box on; grid on
ylim([min_val, max_val]);

% West
vel_mean = [];
vel_std = [];
for t_id = 2:length(data_grid)
    vel_mean(t_id) = data_grid{t_id}.str(1,Wi); %[mean_data, median_data, std_data, n_data]
    vel_std(t_id) = data_grid{t_id}.str(3,Wi); %[mean_data, median_data, std_data, n_data]
end

subplot(3,3,4);
hold on
patch([on_time_point, off_time_point, off_time_point, on_time_point], [min_val, min_val, max_val, max_val], [0.9, 0.9, 0.9], 'FaceAlpha',0.3);
errorbar(vel_mean, vel_std);
yline(0, 'LineWidth', 2);

%legend('exposure', 'ROI', 'whole');
box on; grid on
ylim([min_val, max_val]);

% Centre
vel_mean = [];
vel_std = [];
for t_id = 2:length(data_grid)
    vel_mean(t_id) = data_grid{t_id}.str(1,Ci); %[mean_data, median_data, std_data, n_data]
    vel_std(t_id) = data_grid{t_id}.str(3,Ci); %[mean_data, median_data, std_data, n_data]
end

subplot(3,3,5);
hold on
patch([on_time_point, off_time_point, off_time_point, on_time_point], [min_val, min_val, max_val, max_val], [0.9, 0.9, 0.9], 'FaceAlpha',0.3);
errorbar(vel_mean, vel_std);
yline(0, 'LineWidth', 2);
%legend('exposure', 'ROI', 'whole');
box on; grid on
ylim([min_val, max_val]);

% East
vel_mean = [];
vel_std = [];
for t_id = 2:length(data_grid)
    vel_mean(t_id) = data_grid{t_id}.str(1,Ei); %[mean_data, median_data, std_data, n_data]
    vel_std(t_id) = data_grid{t_id}.str(3,Ei); %[mean_data, median_data, std_data, n_data]
end

subplot(3,3,6);
hold on
patch([on_time_point, off_time_point, off_time_point, on_time_point], [min_val, min_val, max_val, max_val], [0.9, 0.9, 0.9], 'FaceAlpha',0.3);
errorbar(vel_mean, vel_std);
yline(0, 'LineWidth', 2);
%legend('exposure', 'ROI', 'whole');
box on; grid on
ylim([min_val, max_val]);

% South
vel_mean = [];
vel_std = [];
for t_id = 2:length(data_grid)
    vel_mean(t_id) = data_grid{t_id}.str(1,Si); %[mean_data, median_data, std_data, n_data]
    vel_std(t_id) = data_grid{t_id}.str(3,Si); %[mean_data, median_data, std_data, n_data]
end

subplot(3,3,8);
hold on
patch([on_time_point, off_time_point, off_time_point, on_time_point], [min_val, min_val, max_val, max_val], [0.9, 0.9, 0.9], 'FaceAlpha',0.3);
errorbar(vel_mean, vel_std);
yline(0, 'LineWidth', 2);
%legend('exposure', 'ROI', 'whole');
box on; grid on
ylim([min_val, max_val]);
export_fig([out_folder filesep expdate '_' expname  '_NSWE_str.png']);

%% apstr
clf
% North
max_val = 18e-4;
min_val = 0e-4;

vel_mean = [];
vel_std = [];
for t_id = 2:length(data_grid)
    vel_mean(t_id) = data_grid{t_id}.apstr(1,Ni); %[mean_data, median_data, std_data, n_data]
    vel_std(t_id) = data_grid{t_id}.apstr(3,Ni); %[mean_data, median_data, std_data, n_data]
end

subplot(3,3,2);
title('Strain rate');
hold on
patch([on_time_point, off_time_point, off_time_point, on_time_point], [min_val, min_val, max_val, max_val], [0.9, 0.9, 0.9], 'FaceAlpha',0.3);
errorbar(vel_mean, vel_std);
yline(0, 'LineWidth', 2);
%legend('exposure', 'ROI', 'whole');

box on; grid on
ylim([min_val, max_val]);

% West
vel_mean = [];
vel_std = [];
for t_id = 2:length(data_grid)
    vel_mean(t_id) = data_grid{t_id}.apstr(1,Wi); %[mean_data, median_data, std_data, n_data]
    vel_std(t_id) = data_grid{t_id}.apstr(3,Wi); %[mean_data, median_data, std_data, n_data]
end

subplot(3,3,4);
hold on
patch([on_time_point, off_time_point, off_time_point, on_time_point], [min_val, min_val, max_val, max_val], [0.9, 0.9, 0.9], 'FaceAlpha',0.3);
errorbar(vel_mean, vel_std);
yline(0, 'LineWidth', 2);

%legend('exposure', 'ROI', 'whole');
box on; grid on
ylim([min_val, max_val]);

% Centre
vel_mean = [];
vel_std = [];
for t_id = 2:length(data_grid)
    vel_mean(t_id) = data_grid{t_id}.apstr(1,Ci); %[mean_data, median_data, std_data, n_data]
    vel_std(t_id) = data_grid{t_id}.apstr(3,Ci); %[mean_data, median_data, std_data, n_data]
end

subplot(3,3,5);
hold on
patch([on_time_point, off_time_point, off_time_point, on_time_point], [min_val, min_val, max_val, max_val], [0.9, 0.9, 0.9], 'FaceAlpha',0.3);
errorbar(vel_mean, vel_std);
yline(0, 'LineWidth', 2);
%legend('exposure', 'ROI', 'whole');
box on; grid on
ylim([min_val, max_val]);

% East
vel_mean = [];
vel_std = [];
for t_id = 2:length(data_grid)
    vel_mean(t_id) = data_grid{t_id}.apstr(1,Ei); %[mean_data, median_data, std_data, n_data]
    vel_std(t_id) = data_grid{t_id}.apstr(3,Ei); %[mean_data, median_data, std_data, n_data]
end

subplot(3,3,6);
hold on
patch([on_time_point, off_time_point, off_time_point, on_time_point], [min_val, min_val, max_val, max_val], [0.9, 0.9, 0.9], 'FaceAlpha',0.3);
errorbar(vel_mean, vel_std);
yline(0, 'LineWidth', 2);
%legend('exposure', 'ROI', 'whole');
box on; grid on
ylim([min_val, max_val]);

% South
vel_mean = [];
vel_std = [];
for t_id = 2:length(data_grid)
    vel_mean(t_id) = data_grid{t_id}.apstr(1,Si); %[mean_data, median_data, std_data, n_data]
    vel_std(t_id) = data_grid{t_id}.apstr(3,Si); %[mean_data, median_data, std_data, n_data]
end

subplot(3,3,8);
hold on
patch([on_time_point, off_time_point, off_time_point, on_time_point], [min_val, min_val, max_val, max_val], [0.9, 0.9, 0.9], 'FaceAlpha',0.3);
errorbar(vel_mean, vel_std);
yline(0, 'LineWidth', 2);
%legend('exposure', 'ROI', 'whole');
box on; grid on
ylim([min_val, max_val]);
export_fig([out_folder filesep expdate '_' expname 'NSWE_apstr.png']);

%% mlstr
clf
% North
max_val = 18e-4;
min_val = 0e-4;

vel_mean = [];
vel_std = [];
for t_id = 2:length(data_grid)
    vel_mean(t_id) = data_grid{t_id}.mlstr(1,Ni); %[mean_data, median_data, std_data, n_data]
    vel_std(t_id) = data_grid{t_id}.mlstr(3,Ni); %[mean_data, median_data, std_data, n_data]
end

subplot(3,3,2);
title('Strain rate');
hold on
patch([on_time_point, off_time_point, off_time_point, on_time_point], [min_val, min_val, max_val, max_val], [0.9, 0.9, 0.9], 'FaceAlpha',0.3);
errorbar(vel_mean, vel_std);
yline(0, 'LineWidth', 2);
%legend('exposure', 'ROI', 'whole');

box on; grid on
ylim([min_val, max_val]);

% West
vel_mean = [];
vel_std = [];
for t_id = 2:length(data_grid)
    vel_mean(t_id) = data_grid{t_id}.mlstr(1,Wi); %[mean_data, median_data, std_data, n_data]
    vel_std(t_id) = data_grid{t_id}.mlstr(3,Wi); %[mean_data, median_data, std_data, n_data]
end

subplot(3,3,4);
hold on
patch([on_time_point, off_time_point, off_time_point, on_time_point], [min_val, min_val, max_val, max_val], [0.9, 0.9, 0.9], 'FaceAlpha',0.3);
errorbar(vel_mean, vel_std);
yline(0, 'LineWidth', 2);

%legend('exposure', 'ROI', 'whole');
box on; grid on
ylim([min_val, max_val]);

% Centre
vel_mean = [];
vel_std = [];
for t_id = 2:length(data_grid)
    vel_mean(t_id) = data_grid{t_id}.mlstr(1,Ci); %[mean_data, median_data, std_data, n_data]
    vel_std(t_id) = data_grid{t_id}.mlstr(3,Ci); %[mean_data, median_data, std_data, n_data]
end

subplot(3,3,5);
hold on
patch([on_time_point, off_time_point, off_time_point, on_time_point], [min_val, min_val, max_val, max_val], [0.9, 0.9, 0.9], 'FaceAlpha',0.3);
errorbar(vel_mean, vel_std);
yline(0, 'LineWidth', 2);
%legend('exposure', 'ROI', 'whole');
box on; grid on
ylim([min_val, max_val]);

% East
vel_mean = [];
vel_std = [];
for t_id = 2:length(data_grid)
    vel_mean(t_id) = data_grid{t_id}.mlstr(1,Ei); %[mean_data, median_data, std_data, n_data]
    vel_std(t_id) = data_grid{t_id}.mlstr(3,Ei); %[mean_data, median_data, std_data, n_data]
end

subplot(3,3,6);
hold on
patch([on_time_point, off_time_point, off_time_point, on_time_point], [min_val, min_val, max_val, max_val], [0.9, 0.9, 0.9], 'FaceAlpha',0.3);
errorbar(vel_mean, vel_std);
yline(0, 'LineWidth', 2);
%legend('exposure', 'ROI', 'whole');
box on; grid on
ylim([min_val, max_val]);

% South
vel_mean = [];
vel_std = [];
for t_id = 2:length(data_grid)
    vel_mean(t_id) = data_grid{t_id}.mlstr(1,Si); %[mean_data, median_data, std_data, n_data]
    vel_std(t_id) = data_grid{t_id}.mlstr(3,Si); %[mean_data, median_data, std_data, n_data]
end

subplot(3,3,8);
hold on
patch([on_time_point, off_time_point, off_time_point, on_time_point], [min_val, min_val, max_val, max_val], [0.9, 0.9, 0.9], 'FaceAlpha',0.3);
errorbar(vel_mean, vel_std);
yline(0, 'LineWidth', 2);
%legend('exposure', 'ROI', 'whole');
box on; grid on
ylim([min_val, max_val]);
export_fig([out_folder filesep expdate '_' expname 'NSWE_mlstr.png']);


%% iso
clf
% North

max_val = 6e-4;
min_val = -6e-4;

vel_mean = [];
vel_std = [];
for t_id = 2:length(data_grid)
    vel_mean(t_id) = data_grid{t_id}.iso(1,Ni); %[mean_data, median_data, std_data, n_data]
    vel_std(t_id) = data_grid{t_id}.iso(3,Ni); %[mean_data, median_data, std_data, n_data]
end

subplot(3,3,2);
title('Isotropic part');
hold on
patch([on_time_point, off_time_point, off_time_point, on_time_point], [min_val, min_val, max_val, max_val], [0.9, 0.9, 0.9], 'FaceAlpha',0.3);
errorbar(vel_mean, vel_std);
yline(0, 'LineWidth', 2);
%legend('exposure', 'ROI', 'whole');

box on; grid on
ylim([min_val, max_val]);

% West
vel_mean = [];
vel_std = [];
for t_id = 2:length(data_grid)
    vel_mean(t_id) = data_grid{t_id}.iso(1,Wi); %[mean_data, median_data, std_data, n_data]
    vel_std(t_id) = data_grid{t_id}.iso(3,Wi); %[mean_data, median_data, std_data, n_data]
end

subplot(3,3,4);
hold on
patch([on_time_point, off_time_point, off_time_point, on_time_point], [min_val, min_val, max_val, max_val], [0.9, 0.9, 0.9], 'FaceAlpha',0.3);
errorbar(vel_mean, vel_std);
yline(0, 'LineWidth', 2);

%legend('exposure', 'ROI', 'whole');
box on; grid on
ylim([min_val, max_val]);

% Centre
vel_mean = [];
vel_std = [];
for t_id = 2:length(data_grid)
    vel_mean(t_id) = data_grid{t_id}.iso(1,Ci); %[mean_data, median_data, std_data, n_data]
    vel_std(t_id) = data_grid{t_id}.iso(3,Ci); %[mean_data, median_data, std_data, n_data]
end

subplot(3,3,5);
hold on
patch([on_time_point, off_time_point, off_time_point, on_time_point], [min_val, min_val, max_val, max_val], [0.9, 0.9, 0.9], 'FaceAlpha',0.3);
errorbar(vel_mean, vel_std);
yline(0, 'LineWidth', 2);
%legend('exposure', 'ROI', 'whole');
box on; grid on
ylim([min_val, max_val]);

% East
vel_mean = [];
vel_std = [];
for t_id = 2:length(data_grid)
    vel_mean(t_id) = data_grid{t_id}.iso(1,Ei); %[mean_data, median_data, std_data, n_data]
    vel_std(t_id) = data_grid{t_id}.iso(3,Ei); %[mean_data, median_data, std_data, n_data]
end

subplot(3,3,6);
hold on
patch([on_time_point, off_time_point, off_time_point, on_time_point], [min_val, min_val, max_val, max_val], [0.9, 0.9, 0.9], 'FaceAlpha',0.3);
errorbar(vel_mean, vel_std);
yline(0, 'LineWidth', 2);
%legend('exposure', 'ROI', 'whole');
box on; grid on
ylim([min_val, max_val]);

% South
vel_mean = [];
vel_std = [];
for t_id = 2:length(data_grid)
    vel_mean(t_id) = data_grid{t_id}.iso(1,Si); %[mean_data, median_data, std_data, n_data]
    vel_std(t_id) = data_grid{t_id}.iso(3,Si); %[mean_data, median_data, std_data, n_data]
end

subplot(3,3,8);
hold on
patch([on_time_point, off_time_point, off_time_point, on_time_point], [min_val, min_val, max_val, max_val], [0.9, 0.9, 0.9], 'FaceAlpha',0.3);
errorbar(vel_mean, vel_std);
yline(0, 'LineWidth', 2);
%legend('exposure', 'ROI', 'whole');
box on; grid on
ylim([min_val, max_val]);
export_fig([out_folder filesep expdate '_' expname '_NSWE_iso.png']);

%% iso
clf
% North

max_val = 6e-4;
min_val = -6e-4;

vel_mean = [];
vel_std = [];
for t_id = 2:length(data_grid)
    vel_mean(t_id) = data_grid{t_id}.iso_x(1,Ni); %[mean_data, median_data, std_data, n_data]
    vel_std(t_id) = data_grid{t_id}.iso_x(3,Ni); %[mean_data, median_data, std_data, n_data]
end

subplot(3,3,2);
title('Isotropic part');
hold on
patch([on_time_point, off_time_point, off_time_point, on_time_point], [min_val, min_val, max_val, max_val], [0.9, 0.9, 0.9], 'FaceAlpha',0.3);
errorbar(vel_mean, vel_std);
yline(0, 'LineWidth', 2);
%legend('exposure', 'ROI', 'whole');

box on; grid on
ylim([min_val, max_val]);

% West
vel_mean = [];
vel_std = [];
for t_id = 2:length(data_grid)
    vel_mean(t_id) = data_grid{t_id}.iso_x(1,Wi); %[mean_data, median_data, std_data, n_data]
    vel_std(t_id) = data_grid{t_id}.iso_x(3,Wi); %[mean_data, median_data, std_data, n_data]
end

subplot(3,3,4);
hold on
patch([on_time_point, off_time_point, off_time_point, on_time_point], [min_val, min_val, max_val, max_val], [0.9, 0.9, 0.9], 'FaceAlpha',0.3);
errorbar(vel_mean, vel_std);
yline(0, 'LineWidth', 2);

%legend('exposure', 'ROI', 'whole');
box on; grid on
ylim([min_val, max_val]);

% Centre
vel_mean = [];
vel_std = [];
for t_id = 2:length(data_grid)
    vel_mean(t_id) = data_grid{t_id}.iso_x(1,Ci); %[mean_data, median_data, std_data, n_data]
    vel_std(t_id) = data_grid{t_id}.iso_x(3,Ci); %[mean_data, median_data, std_data, n_data]
end

subplot(3,3,5);
hold on
patch([on_time_point, off_time_point, off_time_point, on_time_point], [min_val, min_val, max_val, max_val], [0.9, 0.9, 0.9], 'FaceAlpha',0.3);
errorbar(vel_mean, vel_std);
yline(0, 'LineWidth', 2);
%legend('exposure', 'ROI', 'whole')
box on; grid on
ylim([min_val, max_val]);

% East
vel_mean = [];
vel_std = [];
for t_id = 2:length(data_grid)
    vel_mean(t_id) = data_grid{t_id}.iso_x(1,Ei); %[mean_data, median_data, std_data, n_data]
    vel_std(t_id) = data_grid{t_id}.iso_x(3,Ei); %[mean_data, median_data, std_data, n_data]
end

subplot(3,3,6);
hold on
patch([on_time_point, off_time_point, off_time_point, on_time_point], [min_val, min_val, max_val, max_val], [0.9, 0.9, 0.9], 'FaceAlpha',0.3);
errorbar(vel_mean, vel_std);
yline(0, 'LineWidth', 2);
%legend('exposure', 'ROI', 'whole')
box on; grid on
ylim([min_val, max_val]);

% South
vel_mean = [];
vel_std = [];
for t_id = 2:length(data_grid)
    vel_mean(t_id) = data_grid{t_id}.iso_x(1,Si); %[mean_data, median_data, std_data, n_data]
    vel_std(t_id) = data_grid{t_id}.iso_x(3,Si); %[mean_data, median_data, std_data, n_data]
end

subplot(3,3,8);
hold on
patch([on_time_point, off_time_point, off_time_point, on_time_point], [min_val, min_val, max_val, max_val], [0.9, 0.9, 0.9], 'FaceAlpha',0.3);
errorbar(vel_mean, vel_std);
yline(0, 'LineWidth', 2);
%legend('exposure', 'ROI', 'whole')
box on; grid on
ylim([min_val, max_val]);
export_fig([out_folder filesep expdate '_' expname  '_NSWE_iso_x.png']);

%% iso y
clf
% North

max_val = 6e-4;
min_val = -6e-4;

vel_mean = [];
vel_std = [];
for t_id = 2:length(data_grid)
    vel_mean(t_id) = data_grid{t_id}.iso_y(1,Ni); %[mean_data, median_data, std_data, n_data]
    vel_std(t_id) = data_grid{t_id}.iso_y(3,Ni); %[mean_data, median_data, std_data, n_data]
end

subplot(3,3,2);
title('Isotropic Y part');
hold on
patch([on_time_point, off_time_point, off_time_point, on_time_point], [min_val, min_val, max_val, max_val], [0.9, 0.9, 0.9], 'FaceAlpha',0.3);
errorbar(vel_mean, vel_std);
yline(0, 'LineWidth', 2);
%legend('exposure', 'ROI', 'whole')

box on; grid on
ylim([min_val, max_val]);

% West
vel_mean = [];
vel_std = [];
for t_id = 2:length(data_grid)
    vel_mean(t_id) = data_grid{t_id}.iso_y(1,Wi); %[mean_data, median_data, std_data, n_data]
    vel_std(t_id) = data_grid{t_id}.iso_y(3,Wi); %[mean_data, median_data, std_data, n_data]
end

subplot(3,3,4);
hold on
patch([on_time_point, off_time_point, off_time_point, on_time_point], [min_val, min_val, max_val, max_val], [0.9, 0.9, 0.9], 'FaceAlpha',0.3);
errorbar(vel_mean, vel_std);
yline(0, 'LineWidth', 2);

%legend('exposure', 'ROI', 'whole')
box on; grid on
ylim([min_val, max_val]);

% Centre
vel_mean = [];
vel_std = [];
for t_id = 2:length(data_grid)
    vel_mean(t_id) = data_grid{t_id}.iso_y(1,Ci); %[mean_data, median_data, std_data, n_data]
    vel_std(t_id) = data_grid{t_id}.iso_y(3,Ci); %[mean_data, median_data, std_data, n_data]
end

subplot(3,3,5);
hold on
patch([on_time_point, off_time_point, off_time_point, on_time_point], [min_val, min_val, max_val, max_val], [0.9, 0.9, 0.9], 'FaceAlpha',0.3);
errorbar(vel_mean, vel_std);
yline(0, 'LineWidth', 2);
%legend('exposure', 'ROI', 'whole')
box on; grid on
ylim([min_val, max_val]);

% East
vel_mean = [];
vel_std = [];
for t_id = 2:length(data_grid)
    vel_mean(t_id) = data_grid{t_id}.iso_y(1,Ei); %[mean_data, median_data, std_data, n_data]
    vel_std(t_id) = data_grid{t_id}.iso_y(3,Ei); %[mean_data, median_data, std_data, n_data]
end

subplot(3,3,6);
hold on
patch([on_time_point, off_time_point, off_time_point, on_time_point], [min_val, min_val, max_val, max_val], [0.9, 0.9, 0.9], 'FaceAlpha',0.3);
errorbar(vel_mean, vel_std);
yline(0, 'LineWidth', 2);
%legend('exposure', 'ROI', 'whole')
box on; grid on
ylim([min_val, max_val]);

% South
vel_mean = [];
vel_std = [];
for t_id = 2:length(data_grid)
    vel_mean(t_id) = data_grid{t_id}.iso_y(1,Si); %[mean_data, median_data, std_data, n_data]
    vel_std(t_id) = data_grid{t_id}.iso_y(3,Si); %[mean_data, median_data, std_data, n_data]
end

subplot(3,3,8);
hold on
patch([on_time_point, off_time_point, off_time_point, on_time_point], [min_val, min_val, max_val, max_val], [0.9, 0.9, 0.9], 'FaceAlpha',0.3);
errorbar(vel_mean, vel_std);
yline(0, 'LineWidth', 2);
%legend('exposure', 'ROI', 'whole')
box on; grid on
ylim([min_val, max_val]);
export_fig([out_folder filesep expdate '_' expname  '_NSWE_iso_y.png']);

%% ani
clf
% North
max_val = 8e-4;
min_val = 0;

vel_mean = [];
vel_std = [];
for t_id = 2:length(data_grid)
    vel_mean(t_id) = data_grid{t_id}.ani(1,Ni); %[mean_data, median_data, std_data, n_data]
    vel_std(t_id) = data_grid{t_id}.ani(3,Ni); %[mean_data, median_data, std_data, n_data]
end

subplot(3,3,2);
title('Anisotropic part');
hold on
patch([on_time_point, off_time_point, off_time_point, on_time_point], [min_val, min_val, max_val, max_val], [0.9, 0.9, 0.9], 'FaceAlpha',0.3);
errorbar(vel_mean, vel_std);
yline(0, 'LineWidth', 2);
%legend('exposure', 'ROI', 'whole')

box on; grid on
ylim([min_val, max_val]);

% West
vel_mean = [];
vel_std = [];
for t_id = 2:length(data_grid)
    vel_mean(t_id) = data_grid{t_id}.ani(1,Wi); %[mean_data, median_data, std_data, n_data]
    vel_std(t_id) = data_grid{t_id}.ani(3,Wi); %[mean_data, median_data, std_data, n_data]
end

subplot(3,3,4);
hold on
patch([on_time_point, off_time_point, off_time_point, on_time_point], [min_val, min_val, max_val, max_val], [0.9, 0.9, 0.9], 'FaceAlpha',0.3);
errorbar(vel_mean, vel_std);
yline(0, 'LineWidth', 2);

%legend('exposure', 'ROI', 'whole')
box on; grid on
ylim([min_val, max_val]);

% Centre
vel_mean = [];
vel_std = [];
for t_id = 2:length(data_grid)
    vel_mean(t_id) = data_grid{t_id}.ani(1,Ci); %[mean_data, median_data, std_data, n_data]
    vel_std(t_id) = data_grid{t_id}.ani(3,Ci); %[mean_data, median_data, std_data, n_data]
end

subplot(3,3,5);
hold on
patch([on_time_point, off_time_point, off_time_point, on_time_point], [min_val, min_val, max_val, max_val], [0.9, 0.9, 0.9], 'FaceAlpha',0.3);
errorbar(vel_mean, vel_std);
yline(0, 'LineWidth', 2);
%legend('exposure', 'ROI', 'whole')
box on; grid on
ylim([min_val, max_val]);

% East
vel_mean = [];
vel_std = [];
for t_id = 2:length(data_grid)
    vel_mean(t_id) = data_grid{t_id}.ani(1,Ei); %[mean_data, median_data, std_data, n_data]
    vel_std(t_id) = data_grid{t_id}.ani(3,Ei); %[mean_data, median_data, std_data, n_data]
end

subplot(3,3,6);
hold on
patch([on_time_point, off_time_point, off_time_point, on_time_point], [min_val, min_val, max_val, max_val], [0.9, 0.9, 0.9], 'FaceAlpha',0.3);
errorbar(vel_mean, vel_std);
yline(0, 'LineWidth', 2);
%legend('exposure', 'ROI', 'whole')
box on; grid on
ylim([min_val, max_val]);

% South
vel_mean = [];
vel_std = [];
for t_id = 2:length(data_grid)
    vel_mean(t_id) = data_grid{t_id}.ani(1,Si); %[mean_data, median_data, std_data, n_data]
    vel_std(t_id) = data_grid{t_id}.ani(3,Si); %[mean_data, median_data, std_data, n_data]
end

subplot(3,3,8);
hold on
patch([on_time_point, off_time_point, off_time_point, on_time_point], [min_val, min_val, max_val, max_val], [0.9, 0.9, 0.9], 'FaceAlpha',0.3);
errorbar(vel_mean, vel_std);
yline(0, 'LineWidth', 2);
%legend('exposure', 'ROI', 'whole')
box on; grid on
ylim([min_val, max_val]);
export_fig([out_folder filesep expdate '_' expname  '_NSWE_ani.png']);

%% ani angle
clf
% North
max_val = pi;
min_val = -pi;
vel_mean = [];
vel_std = [];
for t_id = 2:length(data_grid)
    vel_mean(t_id) = data_grid{t_id}.ani_angle(1,Ni); %[mean_data, median_data, std_data, n_data]
    vel_std(t_id) = data_grid{t_id}.ani_angle(3,Ni); %[mean_data, median_data, std_data, n_data]
end

subplot(3,3,2);
title('Anisotropic tensor angle');
hold on
patch([on_time_point, off_time_point, off_time_point, on_time_point], [min_val, min_val, max_val, max_val], [0.9, 0.9, 0.9], 'FaceAlpha',0.3);
errorbar(vel_mean, vel_std);
set(gca,'YTick',-pi:pi/2:pi) ;
set(gca,'YTickLabel',{'-\pi (left)', '-\pi/2 (up)','0 (right)','\pi/2 (down)','\pi (left)'});

%legend('exposure', 'ROI', 'whole')

box on; grid on
ylim([min_val, max_val]);

% West
vel_mean = [];
vel_std = [];
for t_id = 2:length(data_grid)
    vel_mean(t_id) = data_grid{t_id}.ani_angle(1,Wi); %[mean_data, median_data, std_data, n_data]
    vel_std(t_id) = data_grid{t_id}.ani_angle(3,Wi); %[mean_data, median_data, std_data, n_data]
end

subplot(3,3,4);
hold on
patch([on_time_point, off_time_point, off_time_point, on_time_point], [min_val, min_val, max_val, max_val], [0.9, 0.9, 0.9], 'FaceAlpha',0.3);
errorbar(vel_mean, vel_std);
set(gca,'YTick',-pi:pi/2:pi);
set(gca,'YTickLabel',{'-\pi (left)', '-\pi/2 (up)','0 (right)','\pi/2 (down)','\pi (left)'});
%legend('exposure', 'ROI', 'whole')
box on; grid on
ylim([min_val, max_val]);

% Centre
vel_mean = [];
vel_std = [];
for t_id = 2:length(data_grid)
    vel_mean(t_id) = data_grid{t_id}.ani_angle(1,Ci); %[mean_data, median_data, std_data, n_data]
    vel_std(t_id) = data_grid{t_id}.ani_angle(3,Ci); %[mean_data, median_data, std_data, n_data]
end

subplot(3,3,5);
hold on
patch([on_time_point, off_time_point, off_time_point, on_time_point], [min_val, min_val, max_val, max_val], [0.9, 0.9, 0.9], 'FaceAlpha',0.3);
errorbar(vel_mean, vel_std);
set(gca,'YTick',-pi:pi/2:pi);
set(gca,'YTickLabel',{'-\pi (left)', '-\pi/2 (up)','0 (right)','\pi/2 (down)','\pi (left)'});
%legend('exposure', 'ROI', 'whole')
box on; grid on
ylim([min_val, max_val]);

% East
vel_mean = [];
vel_std = [];
for t_id = 2:length(data_grid)
    vel_mean(t_id) = data_grid{t_id}.ani_angle(1,Ei); %[mean_data, median_data, std_data, n_data]
    vel_std(t_id) = data_grid{t_id}.ani_angle(3,Ei); %[mean_data, median_data, std_data, n_data]
end

subplot(3,3,6);
hold on
patch([on_time_point, off_time_point, off_time_point, on_time_point], [min_val, min_val, max_val, max_val], [0.9, 0.9, 0.9], 'FaceAlpha',0.3);
errorbar(vel_mean, vel_std);
set(gca,'YTick',-pi:pi/2:pi);
set(gca,'YTickLabel',{'-\pi (left)', '-\pi/2 (up)','0 (right)','\pi/2 (down)','\pi (left)'});
%legend('exposure', 'ROI', 'whole')
box on; grid on
ylim([min_val, max_val]);

% South
vel_mean = [];
vel_std = [];
for t_id = 2:length(data_grid)
    vel_mean(t_id) = data_grid{t_id}.ani_angle(1,Si); %[mean_data, median_data, std_data, n_data]
    vel_std(t_id) = data_grid{t_id}.ani_angle(3,Si); %[mean_data, median_data, std_data, n_data]
end

subplot(3,3,8);
hold on
patch([on_time_point, off_time_point, off_time_point, on_time_point], [min_val, min_val, max_val, max_val], [0.9, 0.9, 0.9], 'FaceAlpha',0.3);
errorbar(vel_mean, vel_std);
set(gca,'YTick',-pi:pi/2:pi);
set(gca,'YTickLabel',{'-\pi (left)', '-\pi/2 (up)','0 (right)','\pi/2 (down)','\pi (left)'});
%legend('exposure', 'ROI', 'whole')
box on; grid on
ylim([min_val, max_val]);
export_fig([out_folder filesep expdate '_' expname  '_NSWE_ani_angle.png']);

end