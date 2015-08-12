package me.piebridge.prevent.framework;

import android.appwidget.AppWidgetManager;
import android.content.Context;
import android.content.Intent;
import android.content.pm.ApplicationInfo;
import android.content.pm.PackageManager;
import android.content.pm.PackageParser;
import android.net.Uri;
import android.os.Binder;

import java.util.Map;

import me.piebridge.prevent.common.GmsUtils;
import me.piebridge.prevent.common.PackageUtils;
import me.piebridge.prevent.framework.util.BroadcastFilterUtils;
import me.piebridge.prevent.framework.util.LogUtils;
import me.piebridge.prevent.framework.util.NotificationManagerServiceUtils;

/**
 * Created by thom on 15/8/11.
 */
public class IntentFilterHook {

    private static Context mContext;
    private static AccountWatcher accountWatcher;
    private static Map<String, Boolean> mPreventPackages;

    private IntentFilterHook() {

    }

    public static void setContext(Context context, Map<String, Boolean> preventPackages) {
        mContext = context;
        mPreventPackages = preventPackages;
        accountWatcher = new AccountWatcher(mContext);
    }

    public static boolean canHook() {
        return SystemHook.isSystemHook() && mContext != null && accountWatcher != null;
    }

    public static IntentFilterMatchResult hookBeforeMatch(Object filter, Object[] args) {
        String action = (String) args[0];
        Uri data = (Uri) args[0x3];
        if (NotificationManagerServiceUtils.canHook(filter, action)) {
            return NotificationManagerServiceUtils.hook(data, mPreventPackages);
        } else if (filter instanceof PackageParser.ActivityIntentInfo) {
            return hookActivityIntentInfo((PackageParser.ActivityIntentInfo) filter, action);
        } else if (filter instanceof PackageParser.ServiceIntentInfo) {
            return hookServiceIntentInfo((PackageParser.ServiceIntentInfo) filter, action);
        } else if (BroadcastFilterUtils.isBroadcastFilter(filter)) {
            return hookBroadcastFilter(filter, action);
        }

        return IntentFilterMatchResult.NONE;
    }

    private static IntentFilterMatchResult hookBroadcastFilter(Object filter, String action) {
        // for dynamic broadcast, we only disable ACTION_CLOSE_SYSTEM_DIALOGS
        if (!Intent.ACTION_CLOSE_SYSTEM_DIALOGS.equals(action)) {
            return IntentFilterMatchResult.NONE;
        }
        String packageName = BroadcastFilterUtils.getPackageName(filter);
        if (packageName == null) {
            return IntentFilterMatchResult.NONE;
        }
        if (mPreventPackages.containsKey(packageName)) {
            LogUtils.logIntentFilter(true, filter, action, packageName);
            return IntentFilterMatchResult.NO_MATCH;
        }
        return IntentFilterMatchResult.NONE;
    }

    private static IntentFilterMatchResult hookActivityIntentInfo(PackageParser.ActivityIntentInfo filter, String action) {
        // for receiver, we don't block for activity
        PackageParser.Activity activity = filter.activity;
        PackageParser.Package owner = activity.owner;
        if (!owner.receivers.contains(activity)) {
            // we only care about receiver
            return IntentFilterMatchResult.NONE;
        }
        ApplicationInfo ai = owner.applicationInfo;
        if (canNotHook(ai)) {
            return IntentFilterMatchResult.NONE;
        }
        if (Intent.ACTION_CONFIGURATION_CHANGED.equals(action)) {
            // FIXME:
            return IntentFilterMatchResult.NONE;
        }
        // http://developer.android.com/guide/topics/appwidgets/index.html#Manifest
        // http://developer.android.com/reference/android/appwidget/AppWidgetManager.html#ACTION_APPWIDGET_UPDATE
        if (AppWidgetManager.ACTION_APPWIDGET_UPDATE.equals(action)) {
            return IntentFilterMatchResult.NONE;
        }
        return IntentFilterMatchResult.NO_MATCH;
    }

    private static boolean canNotHook(ApplicationInfo ai) {
        boolean prevents = Boolean.TRUE.equals(mPreventPackages.get(ai.packageName));
        return !prevents || canUseGms(ai);
    }

    private static IntentFilterMatchResult hookServiceIntentInfo(PackageParser.ServiceIntentInfo filter, String action) {
        PackageParser.Service service = filter.service;
        PackageParser.Package owner = service.owner;
        ApplicationInfo ai = owner.applicationInfo;
        if (canNotHook(ai)) {
            return IntentFilterMatchResult.NONE;
        }
        String packageName = ai.packageName;
        if (!accountWatcher.canHook(action, packageName)) {
            return IntentFilterMatchResult.NONE;
        }
        if (Binder.getCallingUid() != android.os.Process.SYSTEM_UID) {
            PreventLog.w("filter: " + filter + ", action: " + action + ", packageName: " + packageName + ", callingUid: " + Binder.getCallingUid()
                    + ", callingPid: " + Binder.getCallingPid());
            return IntentFilterMatchResult.NO_MATCH;
        }
        LogUtils.logIntentFilter(false, filter, action, packageName);
        return IntentFilterMatchResult.NONE;
    }

    private static boolean canUseGms(ApplicationInfo info) {
        if (!mPreventPackages.containsKey(GmsUtils.GMS)) {
            return true;
        }
        int uid = info.uid;
        int callingUid = Binder.getCallingUid();
        return callingUid == uid || (GmsUtils.GMS.equals(info.packageName) && canUseGms(callingUid, info.uid));
    }

    public static boolean isSystemPackage(PackageManager pm, String packageName) {
        try {
            ApplicationInfo ai = pm.getApplicationInfo(packageName, 0);
            return PackageUtils.isSystemPackage(ai.flags);
        } catch (PackageManager.NameNotFoundException e) {
            PreventLog.d("cannot find package " + packageName, e);
            return false;
        }
    }

    private static boolean canUseGms(int callingUid, int uid) {
        PackageManager pm = mContext.getPackageManager();
        String[] packageNames = pm.getPackagesForUid(callingUid);
        if (packageNames == null || packageNames.length == 0) {
            return false;
        }
        if (pm.checkSignatures(callingUid, uid) == PackageManager.SIGNATURE_MATCH) {
            if (!isSystemPackage(pm, packageNames[0])) {
                PreventLog.v("allow " + packageNames[0] + "(same signature) with gms to use gms if needed");
            }
            return true;
        } else {
            return packageNames.length == 1 && canUseGms(pm, packageNames[0]);
        }
    }

    private static boolean canUseGms(PackageManager pm, String packageName) {
        if (pm.getLaunchIntentForPackage(packageName) == null || !packageName.startsWith(GmsUtils.GAPPS_PREFIX)) {
            return false;
        } else {
            if (!isSystemPackage(pm, packageName)) {
                PreventLog.v("allow " + packageName + " to use gms if needed");
            }
            return true;
        }
    }

    public static void onPackageAdded() {
        if (accountWatcher != null) {
            accountWatcher.updateAuthDescriptions();
        }
    }

}
