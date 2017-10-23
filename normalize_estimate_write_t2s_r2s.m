list = 'D:\MultiPAMS\sujets_MultiPAMSCtrl.txt'
subjects = textread(list, '%s');
n_subjects = length(subjects);

for subject_index = 1:n_subjects
    
    subject = subjects {subject_index};
    display(['Processing subject ', subject, ' ', num2str(subject_index), ' of ', num2str(n_subjects)]);
    
    to_estimate = {[subject, '_T2S_mean.nii']};
    to_move = {[subject, '_T2S_mean.nii']; [subject, '_r2star.nii']};
    
    spm('defaults','fmri');
    spm_jobman('initcfg');
    
    matlabbatch{1}.spm.spatial.normalise.estwrite.subj.vol = to_estimate;
    matlabbatch{1}.spm.spatial.normalise.estwrite.subj.resample = to_move;
    matlabbatch{1}.spm.spatial.normalise.estwrite.eoptions.biasreg = 0.0001;
    matlabbatch{1}.spm.spatial.normalise.estwrite.eoptions.biasfwhm = 60;
    matlabbatch{1}.spm.spatial.normalise.estwrite.eoptions.tpm = {'D:\spm12\tpm\TPM.nii'};
    matlabbatch{1}.spm.spatial.normalise.estwrite.eoptions.affreg = 'mni';
    matlabbatch{1}.spm.spatial.normalise.estwrite.eoptions.reg = [0 0.001 0.5 0.05 0.2];
    matlabbatch{1}.spm.spatial.normalise.estwrite.eoptions.fwhm = 0;
    matlabbatch{1}.spm.spatial.normalise.estwrite.eoptions.samp = 3;
    matlabbatch{1}.spm.spatial.normalise.estwrite.woptions.bb = [-90 -126 -72
                                                             90 90 108];
    matlabbatch{1}.spm.spatial.normalise.estwrite.woptions.vox = [2 2 2];
    matlabbatch{1}.spm.spatial.normalise.estwrite.woptions.interp = 4;
    matlabbatch{1}.spm.spatial.normalise.estwrite.woptions.prefix = 'w';
    
    spm_jobman('run',matlabbatch);
       
end