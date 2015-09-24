package me.piebridge.prevent.framework;

import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;
import android.content.pm.ApplicationInfo;
import android.content.pm.PackageManager;
import android.content.pm.ResolveInfo;
import android.net.Uri;

import java.lang.reflect.Method;
import java.util.Collection;
import java.util.HashSet;
import java.util.Map;

import me.piebridge.forcestopgb.BuildConfig;
import me.piebridge.prevent.common.GmsUtils;
import me.piebridge.prevent.framework.util.AccountUtils;
import me.piebridge.prevent.framework.util.LogUtils;
import me.piebridge.prevent.framework.util.SafeActionUtils;
import me.piebridge.prevent.framework.util.TaskRecordUtils;

/**
 * Created by thom on 15/8/11.
 */
public class ActivityManagerServiceHook {

    private static Context mContext;
    private static Map<String, Boolean> mPreventPackages;
    // normally, there is only one
    private static Collection<String> settingsPackages = new HashSet<String>();

    private ActivityManagerServiceHook() {

    }

    public static void setContext(Context context, Map<String, Boolean> preventPackages) {
        mContext = context;
        mPreventPackages = preventPackages;
    }

    private static boolean isWantedStartProcessLocked(Class<?>[] types) {
        if (types == null || types.length < 0x6) {
            return false;
        }
        return ApplicationInfo.class.equals(types[0x1])
                && String.class.equals(types[0x4])
                && ComponentName.class.equals(types[0x5]);
    }

    public static Method getStartProcessLocked(Class<?> activityManagerService) {
        Method startProcessLocked = null;
        for (Method method : activityManagerService.getDeclaredMethods()) {
            if (!"startProcessLocked".equals(method.getName()) || !"ProcessRecord".equals(method.getReturnType().getSimpleName()) || !isWantedStartProcessLocked(method.getParameterTypes())) {
                continue;
            }
            if (startProcessLocked == null || startProcessLocked.getParameterTypes().length < method.getParameterTypes().length) {
                startProcessLocked = method;
            }
        }
        return startProcessLocked;
    }

    public static Method getCleanUpRemovedTaskLocked(Class<?> activityManagerService) {
        for (Method method : activityManagerService.getDeclaredMethods()) {
            if ("cleanUpRemovedTaskLocked".equals(method.getName()) && method.getParameterTypes().length == 0x2) {
                return method;
            }
        }
        return null;
    }

    public static boolean hookBeforeStartProcessLocked(Object thiz, Object[] args, String sender) {
        ApplicationInfo info = (ApplicationInfo) args[0x1];
        String hostingType = (String) args[0x4];
        ComponentName hostingName = (ComponentName) args[0x5];
        String packageName = info.packageName;

        if ("content provider".equals(hostingType)) {
            SystemHook.retrievePreventsIfNeeded(thiz);
        }

        if (mContext == null) {
            return true;
        }

        boolean prevents = Boolean.TRUE.equals(mPreventPackages.get(packageName));
        if ("activity".equals(hostingType)) {
            SystemHook.cancelCheck(packageName);
            SystemHook.updateRunningGapps(packageName, true);
            if (prevents) {
                // never block activity
                mPreventPackages.put(packageName, false);
                prevents = false;
            }
            LogUtils.logStartProcess(packageName, hostingType, hostingName, sender);
        }

        return !prevents || hookDependency(hostingName, hostingType, packageName, sender);
    }

    private static boolean hookDependency(ComponentName hostingName, String hostingType, String packageName, String sender) {
        if (packageName.equals(sender)) {
            LogUtils.logStartProcess(packageName, hostingType + "(self)", hostingName, sender);
            return true;
        }
        if ("broadcast".equals(hostingType)) {
            // always block broadcast
            return hookBroadcast(hostingName, hostingType, packageName, sender);
        } else if ("service".equals(hostingType)) {
            return hookService(hostingName, hostingType, packageName, sender);
        } else if ("content provider".equals(hostingType) && !SystemHook.isSystemPackage(packageName)) {
            LogUtils.logStartProcess(true, packageName, hostingType, hostingName, sender);
            return false;
        }

        SystemHook.checkRunningServices(packageName, false);
        LogUtils.logStartProcess(packageName, hostingType + "(should safe)", hostingName, sender);
        return true;
    }

    private static boolean hookBroadcast(ComponentName hostingName, String hostingType, String packageName, String sender) {
        if (SafeActionUtils.isSafeBroadcast(hostingName)) {
            SystemHook.checkRunningServices(packageName, false);
            LogUtils.logStartProcess(packageName, hostingType + "(safe)", hostingName, sender);
            return true;
        } else {
            LogUtils.logStartProcess(true, packageName, hostingType, hostingName, sender);
            return false;
        }
    }

    private static boolean hookService(ComponentName hostingName, String hostingType, String packageName, String sender) {
        if (SafeActionUtils.isSyncService(mContext, hostingName)) {
            return hookSyncService(hostingName, hostingType, packageName, sender);
        }
        if (GmsUtils.isGms(packageName) && !cannotPreventGms(sender)) {
            // only allow gapps to use gms
            LogUtils.logStartProcess(true, packageName, hostingType, hostingName, sender);
            return false;
        }
        if (sender != null && cannotPrevent(sender, packageName)) {
            SystemHook.checkRunningServices(packageName, true);
            LogUtils.logStartProcess(packageName, hostingType, hostingName, sender);
            return true;
        }
        LogUtils.logStartProcess(true, packageName, hostingType, hostingName, sender);
        return false;
    }

    private static void retrieveSettingsPackage(PackageManager pm, Collection<String> packages) {
        Intent intent = new Intent(android.provider.Settings.ACTION_APPLICATION_DETAILS_SETTINGS,
                Uri.fromParts("package", BuildConfig.APPLICATION_ID, null));
        for (ResolveInfo resolveInfo : pm.queryIntentActivities(intent, 0)) {
            String packageName = resolveInfo.activityInfo.packageName;
            if (SystemHook.isSystemPackage(packageName)) {
                PreventLog.d("add " + packageName + " as settings");
                packages.add(packageName);
            }
        }
    }

    private static boolean cannotPreventGms(String sender) {
        if (settingsPackages.isEmpty()) {
            retrieveSettingsPackage(mContext.getPackageManager(), settingsPackages);
        }
        // only allow gapps and settings to use gms
        return GmsUtils.isGapps(mContext.getPackageManager(), sender) || settingsPackages.contains(sender);
    }

    private static boolean cannotPrevent(String sender, String packageName) {
        if (SystemHook.cannotPrevent(sender)) {
            // the sender cannot be prevent
            return true;
        } else if (mContext.getPackageManager().getLaunchIntentForPackage(sender) == null) {
            // allow the sender has no launcher
            return true;
        } else if (SystemHook.isSystemPackage(packageName) && SystemHook.hasRunningActivity(sender)) {
            // allow third-party app to call system package (except gms)
            return true;
        }
        return false;
    }

    private static boolean hookSyncService(ComponentName hostingName, String hostingType, String packageName, String sender) {
        if (AccountUtils.isComponentSyncable(mContext, hostingName)) {
            handleSafeService(packageName);
            SystemHook.checkRunningServices(packageName, true);
            LogUtils.logStartProcess(packageName, hostingType + "(sync)", hostingName, sender);
            return true;
        } else {
            LogUtils.logStartProcess(true, packageName, hostingType + "(sync)", hostingName, sender);
            return false;
        }
    }

    private static void handleSafeService(String packageName) {
        if (Boolean.TRUE.equals(mPreventPackages.get(packageName))) {
            PreventLog.i("allow " + packageName + " for next service/broadcast");
            mPreventPackages.put(packageName, false);
            SystemHook.restoreLater(packageName);
        }
    }

    public static boolean hookAfterCleanUpRemovedTaskLocked(Object[] args) {
        String packageName = TaskRecordUtils.getPackageName(args[0]);
        if (!shouldKillProcess(args[1])) {
            return false;
        }
        SystemHook.updateRunningGapps(packageName, false);
        if (packageName != null && mPreventPackages != null && mPreventPackages.containsKey(packageName)) {
            mPreventPackages.put(packageName, true);
            LogUtils.logForceStop("removeTask", packageName, "force in " + SystemHook.TIME_IMMEDIATE + "s");
            SystemHook.forceStopPackageForce(packageName, SystemHook.TIME_IMMEDIATE);
        }
        return true;
    }

    private static boolean shouldKillProcess(Object killProcess) {
        if (killProcess == null) {
            return false;
        }
        if (killProcess instanceof Boolean) {
            return (Boolean) killProcess;
        } else if (killProcess instanceof Integer) {
            Integer flags = (Integer) killProcess;
            return (flags & 0x1) != 0;
        }
        return false;
    }

}
