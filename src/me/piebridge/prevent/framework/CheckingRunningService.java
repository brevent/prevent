package me.piebridge.prevent.framework;

import android.app.ActivityManager;
import android.content.Context;
import android.content.Intent;

import java.util.Collection;
import java.util.List;
import java.util.Set;
import java.util.TreeSet;

import me.piebridge.forcestopgb.BuildConfig;

/**
 * Created by thom on 15/7/25.
 */

abstract class CheckingRunningService implements Runnable {

    private static final int MAX_SERVICES = 100;

    private final Context mContext;

    CheckingRunningService(Context context) {
        this.mContext = context;
    }

    @Override
    public void run() {
        Collection<String> packageNames = preparePackageNames();
        Collection<String> whiteList = prepareWhiteList();
        ActivityManager activityManager = (ActivityManager) mContext.getSystemService(Context.ACTIVITY_SERVICE);
        List<ActivityManager.RunningServiceInfo> services = activityManager.getRunningServices(MAX_SERVICES);
        if (services == null) {
            return;
        }
        PreventLog.v("services size: " + services.size());
        Set<String> shouldStopPackageNames = new TreeSet<String>();
        for (ActivityManager.RunningServiceInfo service : services) {
            String name = service.service.getPackageName();
            boolean prevent = Boolean.TRUE.equals(SystemHook.getPreventPackages().get(name));
            logServiceIfNeeded(prevent, name, service);
            if (prevent && service.started && !whiteList.contains(name)) {
                if (packageNames.contains(name)) {
                    shouldStopPackageNames.add(name);
                } else if (isPersistentService(service)) {
                    PreventLog.i("package " + name + " has persistent process, will force stop it");
                    shouldStopPackageNames.add(name);
                } else {
                    mContext.stopService(new Intent().setComponent(service.service));
                }
            }
        }
        stopServiceIfNeeded(shouldStopPackageNames);
        PreventLog.v("complete checking running service");
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

}