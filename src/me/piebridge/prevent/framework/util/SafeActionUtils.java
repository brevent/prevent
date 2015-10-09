package me.piebridge.prevent.framework.util;

import android.app.AppGlobals;
import android.appwidget.AppWidgetManager;
import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;
import android.content.pm.ResolveInfo;
import android.content.pm.ServiceInfo;

import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import me.piebridge.prevent.common.PreventIntent;
import me.piebridge.prevent.framework.PreventLog;

/**
 * Created by thom on 15/7/31.
 */
public class SafeActionUtils {

    private static final Object LOCK = new Object();
    private static Map<String, Set<ComponentName>> syncAdapters = new HashMap<String, Set<ComponentName>>();
    private static Map<String, Set<ComponentName>> safeActions = new HashMap<String, Set<ComponentName>>();
    private static Set<ComponentName> widgets = new HashSet<ComponentName>();
    private static Map<String, Collection<String>> SAFE_PACKAGE_ACTIONS = new HashMap<String, Collection<String>>();

    static {
        SAFE_PACKAGE_ACTIONS.put("com.eg.android.AlipayGphone", Collections.singletonList("com.eg.android.AlipayGphone.IAlixPay"));
    }

    private SafeActionUtils() {

    }

    public static void onPackageChanged(String packageName) {
        if (packageName != null) {
            synchronized (LOCK) {
                syncAdapters.remove(packageName);
                safeActions.remove(packageName);
            }
        }
    }

    private static boolean addSafeActions(Map<String, Set<ComponentName>> actions, ComponentName cn) {
        PreventLog.i("add " + cn.flattenToShortString() + " as sync adapter");
        String packageName = cn.getPackageName();
        if (packageName == null) {
            return false;
        }
        Set<ComponentName> components = actions.get(packageName);
        if (components == null) {
            synchronized (LOCK) {
                actions.put(packageName, new HashSet<ComponentName>());
                components = actions.get(packageName);
            }
        }
        components.add(cn);
        return true;
    }

    public static boolean isSafeBroadcast(ComponentName cn) {
        return widgets.contains(cn);
    }

    public static boolean isSyncService(Context context, ComponentName cn) {
        return isSafeActionCache(syncAdapters, cn) || isSyncAdapter(context, cn);
    }

    public static boolean isSyncAdapter(Context context, ComponentName cn) {
        PreventLog.v("check account for service: " + cn.flattenToShortString());
        if (isActionService(context, cn, "android.content.SyncAdapter")) {
            addSafeActions(syncAdapters, cn);
            return true;
        } else {
            return false;
        }
    }

    public static boolean isActionService(Context context, ComponentName cn, String action) {
        Intent intent = new Intent();
        intent.setAction(action);
        intent.setPackage(cn.getPackageName());
        List<ResolveInfo> intentServices = context.getPackageManager().queryIntentServices(intent, 0);
        final int size = intentServices == null ? 0 : intentServices.size();
        for (int i = 0; i < size; ++i) {
            ServiceInfo si = intentServices.get(i).serviceInfo;
            if (new ComponentName(si.packageName, si.name).equals(cn)) {
                return true;
            }
        }
        return false;
    }

    private static boolean isSafeActionCache(Map<String, Set<ComponentName>> actions, ComponentName cn) {
        String packageName = cn.getPackageName();
        if (packageName == null) {
            return false;
        }
        Set<ComponentName> components = actions.get(packageName);
        return components != null && components.contains(cn);
    }

    public static boolean isProtectedBroadcast(String action) {
        if (action == null || AppWidgetManager.ACTION_APPWIDGET_UPDATE.equals(action) || PreventIntent.ACTION_REGISTERED.equals(action)) {
            return false;
        }
        return action.startsWith("android.intent.action") || AppGlobals.getPackageManager().isProtectedBroadcast(action);
    }

    public static void updateWidget(ComponentName component, boolean added) {
        if (component != null) {
            if (added) {
                PreventLog.i("add widget " + component.flattenToShortString());
                widgets.add(component);
            } else {
                PreventLog.i("remove widget " + component.flattenToShortString());
                widgets.remove(component);
            }
        }
    }

    public static boolean cannotPrevent(Context context, ComponentName cn) {
        if (isSafeActionCache(safeActions, cn)) {
            return true;
        }
        Collection<String> actions = SAFE_PACKAGE_ACTIONS.get(cn.getPackageName());
        if (actions == null) {
            return false;
        }
        for (String action : actions) {
            if (isActionService(context, cn, action)) {
                addSafeActions(safeActions, cn);
                return true;
            }
        }
        return false;
    }

    public static boolean isSafeAction(String packageName, String action) {
        Collection<String> actions = SAFE_PACKAGE_ACTIONS.get(packageName);
        return actions != null && actions.contains(action);
    }

}
