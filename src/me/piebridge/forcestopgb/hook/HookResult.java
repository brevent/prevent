package me.piebridge.forcestopgb.hook;

import android.content.IntentFilter;

import me.piebridge.forcestopgb.common.CommonIntent;

/**
 * Created by thom on 15/7/12.
 */
public final class HookResult {

    private Class<?> type;
    private Integer result;

    public static final HookResult MATCH_SCHEME = new HookResult(int.class, IntentFilter.MATCH_CATEGORY_SCHEME | IntentFilter.MATCH_ADJUSTMENT_NORMAL);
    public static final HookResult NONE = new HookResult(Void.class, null);
    public static final HookResult HOOK_ENABLED = new HookResult(int.class, CommonIntent.ACTION_HOOK_ENABLED);
    public static final HookResult NO_MATCH = new HookResult(int.class, IntentFilter.NO_MATCH_ACTION);

    private HookResult(Class<?> _type, Integer _result) {
        type = _type;
        result = _result;
    }

    public boolean isNone() {
        return Void.class.equals(this.type);
    }

    public Integer getResult() {
        return result;
    }

}
