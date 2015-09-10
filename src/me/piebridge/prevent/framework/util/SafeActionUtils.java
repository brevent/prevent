package me.piebridge.prevent.framework.util;

import android.appwidget.AppWidgetManager;
import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;
import android.content.pm.ActivityInfo;
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
        PreventLog.w("add " + cn + " as safe action");
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

    public static boolean isSafeBroadcast(Context context, ComponentName cn) {
        return isSafeComponent(cn) || isWidget(context, cn);
    }

    public static boolean isSafeService(Context context, ComponentName cn) {
        return isSafeComponent(cn) || isAccount(context, cn);
    }

    public static boolean isExported(Context context, ComponentName cn) {
        try {
            PackageManager pm = context.getPackageManager();
            ServiceInfo si = pm.getServiceInfo(cn, 0);
            return si.exported;
        } catch (PackageManager.NameNotFoundException e) {
            PreventLog.d("cannot find " + cn, e);
            return false;
        }
    }

    public static boolean isAccount(Context context, ComponentName cn) {
        PreventLog.v("check account for service: " + cn);
        Intent intent = new Intent();
        intent.setAction("android.content.SyncAdapter");
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

    private static boolean isWidget(Context context, ComponentName cn) {
        PackageManager packageManager = context.getPackageManager();
        Intent intent = new Intent(AppWidgetManager.ACTION_APPWIDGET_UPDATE);
        intent.setPackage(cn.getPackageName());
        List<ResolveInfo> broadcastReceivers = packageManager.queryBroadcastReceivers(intent, 0);
        final int size = broadcastReceivers == null ? 0 : broadcastReceivers.size();
        for (int i = 0; i < size; ++i) {
            ActivityInfo ai = broadcastReceivers.get(i).activityInfo;
            if (new ComponentName(ai.packageName, ai.name).equals(cn)) {
                addSafeAction(cn);
                return true;
            }
        }
        return false;
    }

}
