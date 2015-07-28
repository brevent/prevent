package me.piebridge.prevent.framework;

import android.app.ActivityManager;
import android.content.Context;
import android.content.Intent;
import android.content.pm.PackageManager;

import java.util.List;
import java.util.Set;
import java.util.TreeSet;

import me.piebridge.forcestopgb.BuildConfig;
import me.piebridge.prevent.common.GmsUtils;
import me.piebridge.prevent.common.PackageUtils;

/**
 * Created by thom on 15/7/25.
 */

class CheckingRunningService implements Runnable {

    private static final int MAX_SERVICES = 100;

    private final Context context;
    private final ActivityManager am;
    private final Set<String> checkingPackageNames;


    CheckingRunningService(Context context, ActivityManager am, Set<String> checkingPackageNames) {
        this.am = am;
        this.context = context;
        this.checkingPackageNames = checkingPackageNames;
    }

    @Override
    public void run() {
        Set<String> packageNames = preparePackageNames();
        List<ActivityManager.RunningServiceInfo> services = SystemHook.getActivityManager().getRunningServices(MAX_SERVICES);
        if (services == null) {
            return;
        }
        PreventLog.v("services size: " + services.size());
        PackageManager pm = context.getPackageManager();
        Set<String> shouldStopPackageNames = new TreeSet<String>();
        for (ActivityManager.RunningServiceInfo service : services) {
            String name = service.service.getPackageName();
            boolean isSystem = isSystemPackage(pm, name);
            boolean prevent = Boolean.TRUE.equals(SystemHook.getPreventPackages().get(name));
            logServiceIfNeeded(prevent, name, service);
            if (!isSystem && (service.flags & ActivityManager.RunningServiceInfo.FLAG_PERSISTENT_PROCESS) != 0) {
                PreventLog.i("package " + name + " hash persistent process, force stop it");
                shouldStopPackageNames.add(name);
            } else if (!prevent || !service.started) {
                // do nothing
            } else if (packageNames.contains(name)) {
                shouldStopPackageNames.add(name);
            } else {
                context.stopService(new Intent().setComponent(service.service));
            }
        }
        stopServiceIfNeeded(shouldStopPackageNames);
        PreventLog.v("complete checking running service");
    }

    private Set<String> preparePackageNames() {
        Set<String> packageNames = new TreeSet<String>();
        synchronized (SystemHook.CHECKING_LOCK) {
            packageNames.addAll(checkingPackageNames);
            checkingPackageNames.clear();
        }
        packageNames.remove(GmsUtils.GMS);
        return packageNames;
    }

    private void logServiceIfNeeded(boolean prevents, String name, ActivityManager.RunningServiceInfo service) {
        if (!service.started) {
            return;
        }
        if (BuildConfig.DEBUG || prevents || service.uid >= SystemHook.FIRST_APPLICATION_UID) {
            PreventLog.v("prevents: " + prevents + ", name: " + name + ", count: " + service.clientCount + ", label: " + service.clientLabel
                    + ", uid: " + service.uid + ", pid: " + service.pid + ", process: " + service.process);
        }
    }

    private boolean isSystemPackage(PackageManager pm, String packageName) {
        try {
            return PackageUtils.isSystemPackage(pm.getApplicationInfo(packageName, 0).flags);
        } catch (PackageManager.NameNotFoundException e) {
            PreventLog.d("cannot find package " + packageName, e);
            return false;
        }
    }

    private void stopServiceIfNeeded(Set<String> shouldStopPackageNames) {
        for (String name : shouldStopPackageNames) {
            PreventLog.i(name + " has running services, force stop it");
            SystemHook.forceStopPackage(name);
        }
    }

}