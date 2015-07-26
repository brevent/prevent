package me.piebridge.prevent.framework;

import android.app.ActivityManager;
import android.content.Context;
import android.content.Intent;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import me.piebridge.forcestopgb.BuildConfig;

/**
 * Created by thom on 15/7/25.
 */

class CheckingRunningService implements Runnable {

    private final String packageName;

    private final Map<String, Boolean> serviceStatus;

    CheckingRunningService(String packageName) {
        this.packageName = packageName;
        this.serviceStatus = new HashMap<String, Boolean>();
    }

    @Override
    public void run() {
        List<ActivityManager.RunningServiceInfo> services = SystemHook.getActivityManager().getRunningServices(Integer.MAX_VALUE);
        for (int i = services.size() - 1; i >= 0; --i) {
            ActivityManager.RunningServiceInfo service = services.get(i);
            String name = service.service.getPackageName();
            boolean prevents = Boolean.TRUE.equals(SystemHook.getPreventPackages().get(name));
            if (prevents || BuildConfig.DEBUG) {
                PreventLog.v("prevents: " + prevents + ", name: " + name + ", clientCount: " + service.clientCount
                        + ", started: " + service.started +" , pid: " + service.pid + ", process: " + service.process);
            }
            Context context = SystemHook.getApplication().getApplicationContext();
            if (prevents && (name.equals(this.packageName) || service.uid >= SystemHook.FIRST_APPLICATION_UID)) {
                boolean canStop = service.started;
                if (canStop) {
                    context.stopService(new Intent().setComponent(service.service));
                }
                Boolean result = serviceStatus.get(name);
                if (result == null || result) {
                    serviceStatus.put(name, canStop);
                }
            }
        }
        stopServiceIfNeeded();
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