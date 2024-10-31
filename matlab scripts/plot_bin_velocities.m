function plot_bin_velocities(image_path, mask_file, piv_path, path2roi, out_folder, param, t_bin_seconds)

    pixel_s=param.pixel_s; % [um (/pix)]
    time_step=param.time_step; %[s (/timePoint)]
    spatial_filt_size = param.spatial_filt_size;
    time_filt_size = param.time_filt_size;
    on_time_point = param.on_time_point;
    off_time_point = param.off_time_point;
    
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
    extend = @(x) x(:);
    
    crop = floor(spatial_filt_size/2)+1;
    xlims = [x(1,crop) x(1,end-crop)];
    ylims = [y(crop,1) y(end-crop,1)];
    
    roi = imread(path2roi);
    roi = roi(y(crop,2):y(end-crop,2), x(1,crop):x(1,end-crop));
    roi_perim = bwperim(roi);
    roi_perim = imdilate(roi_perim, strel('disk', 4));
    roi_props = regionprops(roi>1, 'centroid', 'area'); % HC - changed roi>0 to roi>1
    roi_centre = roi_props.Centroid;
    window_side = sqrt(roi_props.Area);

    % get positions so the roi ends up in the center of a window of
    % interest
    nx = (roi_centre(2)-window_side)/window_side;
    ny = (roi_centre(1)-window_side)/window_side;
    xo = round(mod(abs(nx),1)*window_side);
    yo = round(mod(abs(ny),1)*window_side);
    
    vel_thr=0.001;
    thickness=pi/16;


    I = S(:,:,1);
    uM = u;
    vM = v;
    for t_id = 2:size(S,3)-1
        M = ~logical(generate_mask(maskiererx, maskierery, t_id, xroi, yroi, I));
        M = M(y(crop,2):y(end-crop,2), x(1,crop):x(1,end-crop));
        M = imresize(M, [size(u,2), size(u,1)]);
        uM(:,:,t_id) = u(:,:,t_id).*M;
        vM(:,:,t_id) = v(:,:,t_id).*M;
    end        
    
    t_bin_frames = round(t_bin_seconds/time_step);
    
    filename = ['activation_angle_' num2str(t_bin_seconds) '.png'];
    u_avg = mean(uM(:,:,on_time_point:on_time_point + t_bin_frames-1),3);
    v_avg = mean(vM(:,:,on_time_point:on_time_point + t_bin_frames-1),3);
    t_id = round((on_time_point+on_time_point + t_bin_frames-1)/2);
    I = S(:,:,t_id);
    I = I(y(crop,2):y(end-crop,2), x(1,crop):x(1,end-crop));
    I(roi_perim(:)) = 0;
    I = imadjust(I);
    
    plot_angle_velocities(I, xo, yo, window_side, x, y, u_avg, v_avg, t_id, vel_thr, thickness, pixel_s, on_time_point, off_time_point, out_folder, filename)
    
    filename = ['before_angle_' num2str(t_bin_seconds) '.png'];
    u_avg = mean(uM(:,:,on_time_point-t_bin_frames:on_time_point-1),3);
    v_avg = mean(vM(:,:,on_time_point-t_bin_frames:on_time_point-1),3);
    t_id = round((on_time_point-t_bin_frames+on_time_point-1)/2);
    I = S(:,:,t_id);
    I = I(y(crop,2):y(end-crop,2), x(1,crop):x(1,end-crop));
    I(roi_perim(:)) = 0;
    I = imadjust(I);
    
    plot_angle_velocities(I, xo, yo, window_side, x, y, u_avg, v_avg, t_id, vel_thr, thickness, pixel_s, on_time_point, off_time_point, out_folder, filename)
    
    filename = ['after_angle_' num2str(t_bin_seconds) '.png'];
    u_avg = mean(uM(:,:,off_time_point:off_time_point + t_bin_frames-1),3);
    v_avg = mean(vM(:,:,off_time_point:off_time_point + t_bin_frames-1),3);
    t_id = round((off_time_point + off_time_point + t_bin_frames-1)/2);
    I = S(:,:,t_id);
    I = I(y(crop,2):y(end-crop,2), x(1,crop):x(1,end-crop));
    I(roi_perim(:)) = 0;
    I = imadjust(I);
    
    plot_angle_velocities(I, xo, yo, window_side, x, y, u_avg, v_avg, t_id, vel_thr, thickness, pixel_s, on_time_point, off_time_point, out_folder, filename)
    

end