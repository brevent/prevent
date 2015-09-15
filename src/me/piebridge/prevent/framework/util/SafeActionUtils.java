package me.piebridge.prevent.framework.util;

import android.app.AppGlobals;
import android.appwidget.AppWidgetManager;
import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;
import android.content.pm.PackageManager;
import android.content.pm.ResolveInfo;
import android.content.pm.ServiceInfo;

import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import me.piebridge.prevent.framework.PreventLog;

/**
 * Created by thom on 15/7/31.
 */
public class SafeActionUtils {

    private static final Object LOCK = new Object();
    private static Map<String, Set<ComponentName>> safeActions = new HashMap<String, Set<ComponentName>>();
    private static Set<ComponentName> widgets = new HashSet<ComponentName>();

    private SafeActionUtils() {

    }

    public static void onPackageChanged(String packageName) {
        if (packageName != null) {
            synchronized (LOCK) {
                safeActions.remove(packageName);
            }
        }
    }

    private static boolean addSafeAction(ComponentName cn) {
        PreventLog.i("add " + cn + " as safe component");
        String packageName = cn.getPackageName();
        if (packageName == null) {
            return false;
        }
        Set<ComponentName> components = safeActions.get(packageName);
        if (components == null) {
            synchronized (LOCK) {
                safeActions.put(packageName, new HashSet<ComponentName>());
                components = safeActions.get(packageName);
            }
        }
        components.add(cn);
        return true;
    }

    public static boolean isSafeBroadcast(ComponentName cn) {
        return isSafeComponent(cn) || widgets.contains(cn);
    }

    public static boolean isSafeService(Context context, ComponentName cn) {
        return AccountUtils.isPackageSyncable(context, cn.getPackageName()) && (isSafeComponent(cn) || isAccount(context, cn));
    }

    public static boolean isExportedService(Context context, ComponentName cn) {
        try {
            PackageManager pm = context.getPackageManager();
            ServiceInfo si = pm.getServiceInfo(cn, 0);
            return si.exported;
        } catch (PackageManager.NameNotFoundException e) {
            PreventLog.v("cannot find " + cn, e);
            return false;
        }
    }

    public static boolean isAccount(Context context, ComponentName cn) {
        PreventLog.v("check account for service: " + cn);
        return isSafeAccount(context, cn, "android.content.SyncAdapter");
    }

    private static boolean isSafeAccount(Context context, ComponentName cn, String action) {
        Intent intent = new Intent();
        intent.setAction(action);
        intent.setPackage(cn.getPackageName());
        List<ResolveInfo> intentServices = context.getPackageManager().queryIntentServices(intent, 0);
        final int size = intentServices == null ? 0 : intentServices.size();
        for (int i = 0; i < size; ++i) {
            ServiceInfo si = intentServices.get(i).serviceInfo;
            if (new ComponentName(si.packageName, si.name).equals(cn)) {
                addSafeAction(cn);
                return true;
            }
        }
        return false;
    }

    private static boolean isSafeComponent(ComponentName cn) {
        String packageName = cn.getPackageName();
        if (packageName == null) {
            return false;
        }
        Set<ComponentName> components = safeActions.get(packageName);
        return components != null && components.contains(cn);
    }

    public static boolean isProtectedBroadcast(String action) {
        if (action == null || AppWidgetManager.ACTION_APPWIDGET_UPDATE.equals(action)) {
            return false;
        }
        return action.startsWith("android.intent.action") || AppGlobals.getPackageManager().isProtectedBroadcast(action);
    }

    public static void updateWidget(ComponentName component, boolean added) {
        if (component != null) {
            if (added) {
                PreventLog.i("add widget " + component);
                widgets.add(component);
            } else {
                PreventLog.i("remove widget " + component);
                widgets.remove(component);
            }
        }
    }

}
