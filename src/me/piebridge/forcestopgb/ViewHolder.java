package me.piebridge.forcestopgb;

import java.util.Set;

import android.graphics.drawable.Drawable;
import android.widget.CheckBox;
import android.widget.ImageView;
import android.widget.TextView;

public class ViewHolder {
	boolean check;
	String packageName;
	CheckBox checkView;
	ImageView iconView;
	TextView nameView;
	TextView summaryView;
	TextView loadingView;
	ImageView preventView;
	Drawable icon;
	Set<Integer> running;
}