package me.piebridge.prevent.framework;

import android.app.ActivityManager;
import android.content.Context;
import android.content.Intent;

import java.util.Collection;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

import me.piebridge.forcestopgb.BuildConfig;
import me.piebridge.prevent.framework.util.AlarmManagerServiceUtils;
import me.piebridge.prevent.framework.util.HookUtils;

/**
 * Created by thom on 15/7/25.
 */

abstract class CheckingRunningService implements Runnable {

    private final Context mContext;
    private Map<String, Boolean> mPreventPackages;

    CheckingRunningService(Context context, Map<String, Boolean> preventPackages) {
        mContext = context;
        mPreventPackages = preventPackages;
    }

    @Override
    public void run() {
        Collection<String> packageNames = preparePackageNames();
        Collection<String> whiteList = prepareWhiteList();
        if (!packageNames.isEmpty() && packageNames.equals(whiteList)) {
            return;
        }
        Set<String> releaseAlarmPackageNames = new TreeSet<String>();
        Set<String> shouldStopPackageNames = new TreeSet<String>();
        for (ActivityManager.RunningServiceInfo service : HookUtils.getServices(mContext)) {
            checkService(service, packageNames, whiteList, shouldStopPackageNames, releaseAlarmPackageNames);
        }
        stopServiceIfNeeded(shouldStopPackageNames);
        releaseAlarmIfNeeded(releaseAlarmPackageNames, shouldStopPackageNames);
        PreventLog.v("complete checking running service");
    }

    private boolean checkService(ActivityManager.RunningServiceInfo service, Collection<String> packageNames, Collection<String> whiteList, Set<String> shouldStopPackageNames, Set<String> releaseAlarmPackageNames) {
        String name = service.service.getPackageName();
        boolean prevent = Boolean.TRUE.equals(mPreventPackages.get(name));
        logServiceIfNeeded(prevent, name, service);
        if (!prevent || whiteList.contains(name)) {
            return false;
        }
        if (packageNames.contains(name)) {
            if (service.started) {
                shouldStopPackageNames.add(name);
            } else {
                releaseAlarmPackageNames.add(name);
            }
            return true;
        }
        if (!service.started) {
            return true;
        }
        if (isPersistentService(service)) {
            PreventLog.i("package " + name + " has persistent process, will force stop it");
            shouldStopPackageNames.add(name);
        } else {
            mContext.stopService(new Intent().setComponent(service.service));
            releaseAlarmPackageNames.add(name);
        }
        return true;
    }

    protected abstract Collection<String> preparePackageNames();

    protected abstract Collection<String> prepareWhiteList();

    private boolean isPersistentService(ActivityManager.RunningServiceInfo service) {
        return service.process.endsWith(".persistent") || (service.flags & ActivityManager.RunningServiceInfo.FLAG_PERSISTENT_PROCESS) != 0;
    }

    private void logServiceIfNeeded(boolean prevents, String name, ActivityManager.RunningServiceInfo service) {
        if (!service.started) {
            return;
        }
        if (BuildConfig.DEBUG || prevents || service.uid >= SystemHook.FIRST_APPLICATION_UID) {
            PreventLog.v("prevents: " + prevents + ", name: " + name + ", count: " + service.clientCount + ", label: " + service.clientLabel
                    + ", uid: " + service.uid + ", pid: " + service.pid + ", process: " + service.process + ", flags: " + service.flags);
        }
    }

    private void stopServiceIfNeeded(Set<String> shouldStopPackageNames) {
        for (String name : shouldStopPackageNames) {
            PreventLog.i(name + " has running services, force stop it");
            SystemHook.forceStopPackage(name);
        }
    }

    private void releaseAlarmIfNeeded(Set<String> releaseAlarmPackageNames, Set<String> shouldStopPackageNames) {
        for (String name : releaseAlarmPackageNames) {
            if (!shouldStopPackageNames.contains(name)) {
                AlarmManagerServiceUtils.releaseAlarm(name);
            }
        }
    }

}