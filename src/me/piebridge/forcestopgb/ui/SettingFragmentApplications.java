package me.piebridge.forcestopgb.ui;

import java.util.HashSet;
import java.util.Set;

import android.content.pm.ApplicationInfo;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager;

import me.piebridge.forcestopgb.R;

public class SettingFragmentApplications extends SettingFragment {

    private static final int FLAG_SYSTEM_APP = ApplicationInfo.FLAG_SYSTEM | ApplicationInfo.FLAG_UPDATED_SYSTEM_APP;

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
        return (appInfo.flags & FLAG_SYSTEM_APP) != 0 && pm.getLaunchIntentForPackage(appInfo.packageName) == null;
    }

}