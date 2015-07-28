package me.piebridge.prevent.framework;

import android.app.ActivityManager;
import android.content.Context;
import android.content.Intent;
import android.content.pm.PackageManager;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import me.piebridge.forcestopgb.BuildConfig;
import me.piebridge.prevent.ui.util.PackageUtils;

/**
 * Created by thom on 15/7/25.
 */

class CheckingRunningService implements Runnable {

    private final String packageName;

    private final Map<String, Boolean> serviceStatus;

    private static final int MAX_SERVICES = 100;
    private final Context context;
    private final ActivityManager am;

    CheckingRunningService(Context context, ActivityManager am, String packageName) {
        this.context = context;
        this.am = am;
        this.packageName = packageName;
        this.serviceStatus = new HashMap<String, Boolean>();
    }

    @Override
    public void run() {
        PackageManager pm = context.getPackageManager();
        List<ActivityManager.RunningServiceInfo> services = SystemHook.getActivityManager().getRunningServices(MAX_SERVICES);
        if (services == null) {
            return;
        }
        PreventLog.v("services size: " + services.size());
        for (ActivityManager.RunningServiceInfo service : services) {
            String name = service.service.getPackageName();
            boolean isSystem = isSystemPackage(pm, name);
            boolean prevents = Boolean.TRUE.equals(SystemHook.getPreventPackages().get(name));
            if (BuildConfig.DEBUG || prevents || service.uid >= SystemHook.FIRST_APPLICATION_UID) {
                PreventLog.v("prevents: " + prevents + ", name: " + name + ", count: " + service.clientCount + ", label: " + service.clientLabel
                        + ", started: " + service.started + ", uid: " + service.uid + ", pid: " + service.pid + ", process: " + service.process);
            }
            if (name.equals(packageName)) {
                serviceStatus.put(name, true);
            } else if (!isSystem && (service.flags & ActivityManager.RunningServiceInfo.FLAG_PERSISTENT_PROCESS) != 0) {
                PreventLog.i("package " + name + " hash persistent process, force stop it");
                serviceStatus.put(name, true);
            } else if (prevents && service.started) {
                context.stopService(new Intent().setComponent(service.service));
            }
        }
        stopServiceIfNeeded();
        PreventLog.v("complete checking running service");
    }

    private boolean isSystemPackage(PackageManager pm, String packageName) {
        try {
            return PackageUtils.isSystemPackage(pm.getApplicationInfo(packageName, 0).flags);
        } catch (PackageManager.NameNotFoundException e) {
            PreventLog.d("cannot find package " + packageName, e);
            return false;
        }
    }

    private void stopServiceIfNeeded() {
        for (Map.Entry<String, Boolean> entry : serviceStatus.entrySet()) {
            if (entry.getValue()) {
                String name = entry.getKey();
                PreventLog.i(name + " has running services, force stop it");
                SystemHook.forceStopPackage(name);
            }
        }
    }

}