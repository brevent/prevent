package me.piebridge.forcestopgb.ui;

import java.util.HashSet;
import java.util.Set;

import android.content.pm.ApplicationInfo;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager;

import me.piebridge.forcestopgb.R;
import me.piebridge.util.PackageUtils;

public class SettingFragmentApplications extends SettingFragment {

    @Override
    protected Set<String> getPackageNames(SettingActivity activity) {
        Set<String> names = new HashSet<String>();
        PackageManager pm = activity.getPackageManager();
        for (PackageInfo pkgInfo : pm.getInstalledPackages(0)) {
            ApplicationInfo appInfo = pkgInfo.applicationInfo;
            if (appInfo != null && appInfo.enabled && !isSystemNoLauncherApp(appInfo, pm)) {
                names.add(appInfo.packageName);
            }
        }
        return names;
    }

    @Override
    protected boolean canUseCache() {
        return false;
    }

    @Override
    protected int getQueryHint() {
        return R.string.query_hint;
    }

    private boolean isSystemNoLauncherApp(ApplicationInfo appInfo, PackageManager pm) {
        return PackageUtils.isSystemPackage(appInfo.flags) && pm.getLaunchIntentForPackage(appInfo.packageName) == null;
    }

}