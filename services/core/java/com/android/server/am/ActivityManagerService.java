package com.android.server.am;

import android.app.IApplicationThread;
import android.app.IServiceConnection;
import android.app.ProfilerInfo;
import android.content.ComponentName;
import android.content.Context;
import android.content.IIntentReceiver;
import android.content.Intent;
import android.content.pm.ApplicationInfo;
import android.os.Bundle;
import android.os.IBinder;
import android.os.ParcelFileDescriptor;
import android.os.TransactionTooLargeException;

/**
 * Created by thom on 2016/10/23.
 */
public abstract class ActivityManagerService implements IBinder {

    Context mContext;

    ProcessRecord getRecordForAppLocked(IApplicationThread caller) {
        throw new UnsupportedOperationException();
    }

    // 24, 23, 22, 21
    final ProcessRecord startProcessLocked(String processName, ApplicationInfo info,
                                           boolean knownToBeDead, int intentFlags, String hostingType, ComponentName hostingName,
                                           boolean allowWhileBooting, boolean isolated, int isolatedUid, boolean keepIfLarge,
                                           String abiOverride, String entryPoint, String[] entryPointArgs, Runnable crashHandler) {
        if (PreventRunningUtils.hookStartProcessLocked(processName, info, knownToBeDead, intentFlags, hostingType, hostingName)) {
            return startProcessLocked$Pr(processName, info,
                    knownToBeDead, intentFlags, hostingType, hostingName,
                    allowWhileBooting, isolated, isolatedUid, keepIfLarge,
                    abiOverride, entryPoint, entryPointArgs, crashHandler);
        } else {
            return null;
        }
    }

    // 19
    final ProcessRecord startProcessLocked(String processName,
                                           ApplicationInfo info, boolean knownToBeDead, int intentFlags,
                                           String hostingType, ComponentName hostingName, boolean allowWhileBooting,
                                           boolean isolated, boolean keepIfLarge) {
        if (PreventRunningUtils.hookStartProcessLocked(processName, info, knownToBeDead, intentFlags, hostingType, hostingName)) {
            return startProcessLocked$Pr(processName,
                    info, knownToBeDead, intentFlags,
                    hostingType, hostingName, allowWhileBooting,
                    isolated, keepIfLarge);
        } else {
            return null;
        }
    }

    // 24, 23, 22, 21
    public final int startActivity(IApplicationThread caller, String callingPackage,
                                   Intent intent, String resolvedType, IBinder resultTo, String resultWho, int requestCode,
                                   int startFlags, ProfilerInfo profilerInfo, Bundle bOptions) {
        return PreventRunningUtils.onStartActivity(
                startActivity$Pr(caller, callingPackage,
                        intent, resolvedType, resultTo, resultWho, requestCode,
                        startFlags, profilerInfo, bOptions),
                caller, callingPackage, intent);
    }

    // 19, 18
    public final int startActivity(IApplicationThread caller, String callingPackage,
                                   Intent intent, String resolvedType, IBinder resultTo,
                                   String resultWho, int requestCode, int startFlags,
                                   String profileFile, ParcelFileDescriptor profileFd, Bundle options) {
        return PreventRunningUtils.onStartActivity(
                startActivity$Pr(caller, callingPackage,
                        intent, resolvedType, resultTo,
                        resultWho, requestCode, startFlags,
                        profileFile, profileFd, options),
                caller, callingPackage, intent);
    }

    // 24, 23, 22, 21, 19, 18, 17, 16, 15, 14
    // check "has died"
    private final void handleAppDiedLocked(ProcessRecord app,
                                           boolean restarting, boolean allowRestart) {
        handleAppDiedLocked$Pr(app, restarting, allowRestart);
        if (!restarting && allowRestart && !app.killedByAm) {
            PreventRunningUtils.onAppDied(app);
        }
    }

    // 24
    // check tr.getBaseIntent
    private void cleanUpRemovedTaskLocked(TaskRecord tr, boolean killProcess,
                                          boolean removeFromRecents) {
        try {
            cleanUpRemovedTaskLocked$Pr(tr, killProcess, removeFromRecents);
        } finally {
            if (killProcess) {
                PreventRunningUtils.onCleanUpRemovedTask(tr.getBaseIntent());
            }
        }
    }

    // 23, 22
    // check tr.getBaseIntent
    private void cleanUpRemovedTaskLocked(TaskRecord tr, boolean killProcess) {
        try {
            cleanUpRemovedTaskLocked$Pr(tr, killProcess);
        } finally {
            if (killProcess) {
                PreventRunningUtils.onCleanUpRemovedTask(tr.getBaseIntent());
            }
        }
    }

    // 21, 19, 18, 17, 16
    // check baseIntent
    private void cleanUpRemovedTaskLocked(TaskRecord tr, int flags) {
        try {
            cleanUpRemovedTaskLocked$Pr(tr, flags);
        } finally {
            // REMOVE_TASK_KILL_PROCESS = 0x0001
            if ((flags & 0x0001) != 0) {
                Intent baseIntent = new Intent(tr.intent != null ? tr.intent : tr.affinityIntent);
                PreventRunningUtils.onCleanUpRemovedTask(baseIntent);
            }
        }
    }

    // 24, 23, 22, 21, 19, 18, 17, 16, 15, 14, 10
    public boolean moveActivityTaskToBack(IBinder token, boolean nonRoot) {
        if (moveActivityTaskToBack$Pr(token, nonRoot)) {
            PreventRunningUtils.onMoveActivityTaskToBack(token);
            return true;
        } else {
            return false;
        }
    }

    // 24, 23
    public ComponentName startService(IApplicationThread caller, Intent service,
                                      String resolvedType, String callingPackage, int userId)
            throws TransactionTooLargeException {
        try {
            PreventRunningUtils.setSender(caller);
            if (PreventRunningUtils.hookStartService(caller, service)) {
                return startService$Pr(caller, service, resolvedType, callingPackage, userId);
            }
            return null;
        } finally {
            PreventRunningUtils.clearSender();
        }
    }

    // 22, 21, 19, 18
    public ComponentName startService(IApplicationThread caller, Intent service,
                                      String resolvedType, int userId) {
        try {
            PreventRunningUtils.setSender(caller);
            if (PreventRunningUtils.hookStartService(caller, service)) {
                return startService$Pr(caller, service, resolvedType, userId);
            }
            return null;
        } finally {
            PreventRunningUtils.clearSender();
        }
    }

    // 24, 23
    public int bindService(IApplicationThread caller, IBinder token, Intent service,
                           String resolvedType, IServiceConnection connection, int flags, String callingPackage,
                           int userId) throws TransactionTooLargeException {
        try {
            PreventRunningUtils.setSender(caller);
            if (PreventRunningUtils.hookBindService(caller, token, service)) {
                return bindService$Pr(caller, token, service,
                        resolvedType, connection, flags, callingPackage, userId);
            } else {
                return 0;
            }
        } finally {
            PreventRunningUtils.clearSender();
        }
    }

    // 22, 21, 19, 18, 17, 16
    public int bindService(IApplicationThread caller, IBinder token,
                           Intent service, String resolvedType,
                           IServiceConnection connection, int flags, int userId) {
        try {
            PreventRunningUtils.setSender(caller);
            if (PreventRunningUtils.hookBindService(caller, token, service)) {
                return bindService$Pr(caller, token, service,
                        resolvedType, connection, flags, userId);
            } else {
                return 0;
            }
        } finally {
            PreventRunningUtils.clearSender();
        }
    }

    // 24, 23
    public final int broadcastIntent(IApplicationThread caller,
                                     Intent intent, String resolvedType, IIntentReceiver resultTo,
                                     int resultCode, String resultData, Bundle resultExtras,
                                     String[] requiredPermissions, int appOp, Bundle bOptions,
                                     boolean serialized, boolean sticky, int userId) {
        try {
            PreventRunningUtils.setSender(caller);
            int res = broadcastIntent$Pr(caller,
                    intent, resolvedType, resultTo,
                    resultCode, resultData, resultExtras,
                    requiredPermissions, appOp, bOptions,
                    serialized, sticky, userId);
            if (res == 0) {
                PreventRunningUtils.onBroadcastIntent(intent);
            }
            return res;
        } finally {
            PreventRunningUtils.clearSender();
        }
    }

    // 22, 21, 19, 18,
    public final int broadcastIntent(IApplicationThread caller,
                                     Intent intent, String resolvedType, IIntentReceiver resultTo,
                                     int resultCode, String resultData, Bundle map,
                                     String requiredPermission, int appOp, boolean serialized, boolean sticky, int userId) {
        try {
            PreventRunningUtils.setSender(caller);
            int res = broadcastIntent$Pr(caller,
                    intent, resolvedType, resultTo,
                    resultCode, resultData, map,
                    requiredPermission, appOp, serialized, sticky, userId);
            if (res == 0) {
                PreventRunningUtils.onBroadcastIntent(intent);
            }
            return res;
        } finally {
            PreventRunningUtils.clearSender();
        }
    }

    final ProcessRecord startProcessLocked$Pr(String processName, ApplicationInfo info,
                                              boolean knownToBeDead, int intentFlags, String hostingType, ComponentName hostingName,
                                              boolean allowWhileBooting, boolean isolated, int isolatedUid, boolean keepIfLarge,
                                              String abiOverride, String entryPoint, String[] entryPointArgs, Runnable crashHandler) {
        throw new UnsupportedOperationException();
    }

    final ProcessRecord startProcessLocked$Pr(String processName,
                                              ApplicationInfo info, boolean knownToBeDead, int intentFlags,
                                              String hostingType, ComponentName hostingName, boolean allowWhileBooting,
                                              boolean isolated, boolean keepIfLarge) {
        throw new UnsupportedOperationException();
    }

    public final int startActivity$Pr(IApplicationThread caller, String callingPackage,
                                      Intent intent, String resolvedType, IBinder resultTo, String resultWho, int requestCode,
                                      int startFlags, ProfilerInfo profilerInfo, Bundle bOptions) {
        throw new UnsupportedOperationException();
    }

    public final int startActivity$Pr(IApplicationThread caller, String callingPackage,
                                      Intent intent, String resolvedType, IBinder resultTo,
                                      String resultWho, int requestCode, int startFlags,
                                      String profileFile, ParcelFileDescriptor profileFd, Bundle options) {

        throw new UnsupportedOperationException();
    }

    private final void handleAppDiedLocked$Pr(ProcessRecord app,
                                                boolean restarting, boolean allowRestart) {
        throw new UnsupportedOperationException();
    }

    private void cleanUpRemovedTaskLocked$Pr(TaskRecord tr, boolean killProcess,
                                               boolean removeFromRecents) {
        throw new UnsupportedOperationException();
    }

    private void cleanUpRemovedTaskLocked$Pr(TaskRecord tr, boolean killProcess) {
        throw new UnsupportedOperationException();
    }

    private void cleanUpRemovedTaskLocked$Pr(TaskRecord tr, int flags) {
        throw new UnsupportedOperationException();
    }

    public boolean moveActivityTaskToBack$Pr(IBinder token, boolean nonRoot) {
        throw new UnsupportedOperationException();
    }

    public ComponentName startService$Pr(IApplicationThread caller, Intent service,
                                         String resolvedType, String callingPackage, int userId)
            throws TransactionTooLargeException {
        throw new UnsupportedOperationException();
    }

    public ComponentName startService$Pr(IApplicationThread caller, Intent service,
                                         String resolvedType, int userId) {
        throw new UnsupportedOperationException();
    }

    public int bindService$Pr(IApplicationThread caller, IBinder token, Intent service,
                              String resolvedType, IServiceConnection connection, int flags, String callingPackage,
                              int userId) throws TransactionTooLargeException {
        throw new UnsupportedOperationException();
    }

    public int bindService$Pr(IApplicationThread caller, IBinder token,
                              Intent service, String resolvedType,
                              IServiceConnection connection, int flags, int userId) {
        throw new UnsupportedOperationException();
    }

    public final int broadcastIntent$Pr(IApplicationThread caller,
                                        Intent intent, String resolvedType, IIntentReceiver resultTo,
                                        int resultCode, String resultData, Bundle resultExtras,
                                        String[] requiredPermissions, int appOp, Bundle bOptions,
                                        boolean serialized, boolean sticky, int userId) {
        throw new UnsupportedOperationException();
    }

    public final int broadcastIntent$Pr(IApplicationThread caller,
                                        Intent intent, String resolvedType, IIntentReceiver resultTo,
                                        int resultCode, String resultData, Bundle map,
                                        String requiredPermission, int appOp, boolean serialized, boolean sticky, int userId) {
        throw new UnsupportedOperationException();
    }

}
