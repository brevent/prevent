package me.piebridge.forcestopgb.ui;

import java.util.HashSet;
import java.util.Set;

import android.content.pm.ApplicationInfo;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager;

import me.piebridge.forcestopgb.R;

public class SettingFragmentApplications extends SettingFragment {

    @Override
    protected Set<String> getPackageNames(SettingActivity activity) {
        Set<String> names = new HashSet<String>();
        PackageManager pm = activity.getPackageManager();
        for (PackageInfo pkgInfo : pm.getInstalledPackages(0)) {
            ApplicationInfo appInfo = pkgInfo.applicationInfo;
            if (appInfo == null) {
                continue;
            }
            if (!appInfo.enabled) {
                continue;
            }
            if ((appInfo.flags & ApplicationInfo.FLAG_SYSTEM) != 0 && pm.getLaunchIntentForPackage(appInfo.packageName) == null) {
                continue;
            }
            names.add(appInfo.packageName);
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

}