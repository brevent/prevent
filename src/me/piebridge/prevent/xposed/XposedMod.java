package me.piebridge.prevent.xposed;

import android.app.ActivityThread;

import de.robv.android.xposed.IXposedHookZygoteInit;
import de.robv.android.xposed.XposedBridge;
import me.piebridge.forcestopgb.BuildConfig;
import me.piebridge.prevent.framework.PreventLog;

public class XposedMod implements IXposedHookZygoteInit {

    @Override
    public void initZygote(IXposedHookZygoteInit.StartupParam startupParam) throws Throwable {
        PreventLog.i("prevent running " + BuildConfig.VERSION_NAME);
        XposedBridge.hookAllMethods(ActivityThread.class, "systemMain", new SystemServiceHook());
    }

}
