list = 'D:\MultiPAMS\sujets_MultiPAMSCtrl.txt'

subjects = textread(list, '%s');

n_subjects = length(subjects);

for subject_index = 1:n_subjects
    
    display(['Processing subject ', num2str(subject_index), ' of ', num2str(n_subjects)])
    
    subject = subjects {subject_index};
    
       
    to_invert = {['y_', subject, '_T2S_mean.nii']}
    image_defining_space = {[subject, '_r2star.nii']}
    def_out_name = [subject, '_inverted_field']
    final_name_of_seed = [subject, '_in_subject_space']

    
    spm('defaults','fmri');
    spm_jobman('initcfg');

    matlabbatch{1}.spm.util.defs.comp{1}.inv.comp{1}.def = to_invert;
    matlabbatch{1}.spm.util.defs.comp{1}.inv.space = image_defining_space;
    matlabbatch{1}.spm.util.defs.out{1}.savedef.ofname = def_out_name;
    matlabbatch{1}.spm.util.defs.out{1}.savedef.savedir.savepwd = 1;
    matlabbatch{2}.spm.spatial.normalise.write.subj.def(1) = cfg_dep('Deformations: Deformation', substruct('.','val', '{}',{1}, '.','val', '{}',{1}, '.','val', '{}',{1}), substruct('.','def'));
    matlabbatch{2}.spm.spatial.normalise.write.subj.resample = {'D:\MultiPAMS\rs-fmri\ROIs\sn_bilateral_Pat.nii,1'};
    matlabbatch{2}.spm.spatial.normalise.write.woptions.bb = [NaN NaN NaN
                                                          NaN NaN NaN];
    matlabbatch{2}.spm.spatial.normalise.write.woptions.vox = [NaN NaN NaN];
    matlabbatch{2}.spm.spatial.normalise.write.woptions.interp = 0;
    matlabbatch{2}.spm.spatial.normalise.write.woptions.prefix = final_name_of_seed;
    spm_jobman('run',matlabbatch);
    
end