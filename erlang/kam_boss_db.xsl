<?xml version='1.0'?>
<!--
 * $Id: dbtext.xsl 4518 2008-07-28 15:39:28Z henningw $
 *
 * XSL converter script for boss_db
 *
 * Copyright (C) 2014 stangrze@yahoo.com
 *
 * This file is part of Kamailio, a free SIP server.
 *
 * Kamailio is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version
 *
 * Kamailio is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License 
 * along with this program; if not, write to the Free Software 
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 */
-->


<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
                version='1.0'
                xmlns:xi="http://www.w3.org/2001/XInclude">

    <xsl:key name="column_id" match="column" use="@id|xml:id"/>

    <xsl:param name="prefix" select="_"/>
    <xsl:param name="dir" select="mm"/>
    <xsl:param name="db" select="_"/>

    <xsl:variable name="sign-prefix">unsigned </xsl:variable>

    <!-- Do not output text -->
    <xsl:template match="text()|@*"/>
    <xsl:template match="text()|@*" mode="drop"/>

    <xsl:template name="Capitalize">
        <xsl:param name="word" select="."/>
        <xsl:value-of select="concat(
            translate(substring($word, 1, 1),'abcdefghijklmnopqrstuvwxyz','ABCDEFGHIJKLMNOPQRSTUVWXYZ'),
            translate(substring($word, 2),'ABCDEFGHIJKLMNOPQRSTUVWXYZ','abcdefghijklmnopqrstuvwxyz')
        )"/>
    </xsl:template>
    <xsl:template name="get-name">
        <xsl:param name="select" select="."/>
        <xsl:choose>
            <!-- override test -->
            <xsl:when test="count($select/name[@db=$db])='1'">
                <xsl:value-of select="normalize-space($select/name[@db=$db])"/>
            </xsl:when>
            <!-- No override, use the standard name -->
            <xsl:otherwise>
                <xsl:value-of select="normalize-space($select/name)"/>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>
    <xsl:template name="type-error">
        <xsl:message terminate="yes">
            <xsl:text>ERROR: Table: </xsl:text>
            <xsl:value-of select="normalize-space(parent::table/name)"/>
            <xsl:text>, column: </xsl:text>
            <xsl:value-of select="normalize-space(name)"/>
            <xsl:text> - unsupported column type: </xsl:text>
            <xsl:value-of select="normalize-space(type)"/>
            <xsl:text>.</xsl:text>
        </xsl:message>
    </xsl:template>

    <!-- Process the root database element -->
    <xsl:template match="/">
        <xsl:apply-templates select="database[1]"/>
    </xsl:template>

   <!-- ################ DATABASE ################# -->
    <xsl:template match="database">
        <xsl:apply-templates select="table"/>
        <xsl:apply-templates select="user"/>
    </xsl:template>

    <!--xsl:template match="database" mode="data">
        <xsl:apply-templates select="table" mode="data"/>
    </xsl:template -->
    <!-- ################ /DATABASE ################# -->

    <!-- ################ TABLE ################# -->
    <xsl:template match="table">
	<xsl:variable name="name">
	    <xsl:call-template name="get-name"/>
	</xsl:variable>
	<xsl:variable name="path" select="concat($dir, concat('/', concat( $name,'.erl')))"/>
	<xsl:document href="{$path}" method="text" indent="no" omit-xml-declaration="yes">
		<xsl:text>-module(</xsl:text>
		<xsl:call-template name="get-name"/>
		<xsl:text>, [Id, </xsl:text>
		<xsl:apply-templates select="column" mode="list"/>
		<xsl:text>]).&#x0A;</xsl:text>
		<xsl:text>-compile(export_all).&#x0A;</xsl:text>
		<xsl:text>-columns([{id,"id"}, </xsl:text>
		<xsl:apply-templates select="column" mode="column-list"/>
		<xsl:text>]).</xsl:text>
		<xsl:text>&#x0A;</xsl:text>
		<xsl:text>-table("</xsl:text>
		<xsl:call-template name="get-name"/>
		<xsl:text>").</xsl:text>
		<xsl:text>&#x0A;</xsl:text>
		<!--xsl:apply-templates select="column"/>
		<xsl:apply-templates select="index"/>
		<xsl:apply-templates select="version"/-->
	</xsl:document>
    </xsl:template>

    <!-- ################ /TABLE ################# -->

    <!-- ################ COLUMN ################# -->
    <xsl:template match="column" mode="list">
	<xsl:variable name="name">
	    <xsl:call-template name="get-name"/>
	</xsl:variable>
	<xsl:if test="not($name='id')">
	    <xsl:choose>
		<xsl:when test="($name='instance')">
		    <xsl:text>Instanc</xsl:text>
		</xsl:when>
		<xsl:otherwise>
		    <xsl:call-template name="Capitalize">
			<xsl:with-param name="word" select="name"/>
		    </xsl:call-template>
		</xsl:otherwise>
	    </xsl:choose>
	    <xsl:text></xsl:text>
	    <xsl:variable name="type">
		<xsl:call-template name="get-type"/>
	    </xsl:variable>
	    <xsl:choose>
		<xsl:when test="type[@db=$db]">
		    <xsl:value-of select="normalize-space(type[@db=$db])"/>
		</xsl:when>
		<xsl:when test="$type='char' or 
						$type='short' or 
						$type='int' or
						$type='long'">
		    <!--xsl:text>integer</xsl:text-->
		</xsl:when>
		<xsl:when test="$type='datetime'">
		    <xsl:text>::datetime()</xsl:text>
		</xsl:when>
		<xsl:when test="$type='float' or 
						$type='double'">
		    <xsl:text>::float()</xsl:text>
		</xsl:when>
		<xsl:when test="$type='string' or
						$type='text' or
						$type='binary' or 
						$type='largetext' or
						$type='largebinary'">
		    <xsl:text>::string()</xsl:text>
		</xsl:when>
		<xsl:otherwise>
		    <xsl:call-template name="type-error"/>
		</xsl:otherwise>
	    </xsl:choose>
	    <xsl:if test="not(position()=last())">
		<xsl:text>, </xsl:text>
	    </xsl:if>
	</xsl:if>
    </xsl:template>
    <xsl:template match="column" mode="column-list">
	<xsl:variable name="name">
	    <xsl:call-template name="get-name"/>
	</xsl:variable>
	<xsl:text>{</xsl:text>
	<xsl:choose>
	    <xsl:when test="($name='instance')">
		<xsl:text>instanc</xsl:text>
	    </xsl:when>
	    <xsl:otherwise>
		<xsl:call-template name="get-name"/>
	    </xsl:otherwise>
	</xsl:choose>
	<xsl:text>, "</xsl:text>
	<xsl:call-template name="get-name"/>
	<xsl:text>"}</xsl:text>
        <xsl:if test="not(position()=last())">
            <xsl:text>, </xsl:text>
        </xsl:if>
    </xsl:template>
    <xsl:template match="column">
	<xsl:variable name="type">
	    <xsl:call-template name="get-type"/>
	</xsl:variable>

	<xsl:variable name="null">
	    <xsl:call-template name="get-null"/>
	</xsl:variable>

	<xsl:call-template name="get-name"/>
	<xsl:text>(</xsl:text>
	<xsl:choose>
	    <xsl:when test="type[@db=$db]">
		<xsl:value-of select="normalize-space(type[@db=$db])"/>
	    </xsl:when>
	    <xsl:when test="$type='char' or 
						$type='short' or 
						$type='int' or
						$type='long' or 
						$type='datetime'">
		<xsl:text>integer()</xsl:text>
	    </xsl:when>
	    <xsl:when test="$type='datetime'">
		<xsl:text>datetime()</xsl:text>
	    </xsl:when>
	    <xsl:when test="$type='float' or 
						$type='double'">
		<xsl:text>float()</xsl:text>
	    </xsl:when>
	    <xsl:when test="$type='string' or
						$type='text' or
						$type='binary' or 
						$type='largetext' or
						$type='largebinary'">
		<xsl:text>string()</xsl:text>
	    </xsl:when>
	    <xsl:otherwise>
		<xsl:call-template name="type-error"/>
	    </xsl:otherwise>
	</xsl:choose>

	<xsl:if test="$null=1">
	    <xsl:text>,null</xsl:text>
	</xsl:if>
	<xsl:text>) </xsl:text>
	<xsl:if test="position()=last()">
	    <xsl:text>&#x0A;</xsl:text>
	</xsl:if>
    </xsl:template>
    <xsl:template name="get-type-string">
        <xsl:param name="select" select="."/>
        <xsl:choose>
            <xsl:when test="count($select/type[@db=$db])='1'">
                <xsl:value-of select="translate(normalize-space($select/type[@db=$db]),
                  'ABCDEFGHIJKLMNOPQRSTUVWXYZ', 'abcdefghijklmnopqrstuvwxyz')"/>
            </xsl:when>
            <xsl:otherwise>
                <xsl:value-of select="translate(normalize-space($select/type),
                  'ABCDEFGHIJKLMNOPQRSTUVWXYZ', 'abcdefghijklmnopqrstuvwxyz')"/>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>
    <xsl:template name="get-type">
        <xsl:param name="select" select="."/>
        <xsl:variable name="type">
            <xsl:call-template name="get-type-string">
                <xsl:with-param name="select" select="$select"/>
            </xsl:call-template>
        </xsl:variable>
        <xsl:choose>
            <xsl:when test="starts-with($type, $sign-prefix)">
                <xsl:value-of select="substring-after($type, $sign-prefix)"/>
            </xsl:when>
            <xsl:otherwise>
                <xsl:value-of select="$type"/>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>
    <xsl:template name="get-sign">
        <xsl:param name="select" select="."/>
        <xsl:variable name="type">
            <xsl:call-template name="get-type-string">
                <xsl:with-param name="select" select="$select"/>
            </xsl:call-template>
        </xsl:variable>
        <xsl:choose>
            <xsl:when test="starts-with($type, $sign-prefix)">0</xsl:when>
            <xsl:otherwise>1</xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <xsl:template name="get-null">
        <xsl:param name="select" select="."/>
        <xsl:choose>
            <xsl:when test="count($select/null[@db=$db])='1'">1</xsl:when>
            <xsl:when test="count($select/null)='1'">1</xsl:when>
            <xsl:otherwise>0</xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <xsl:template name="get-size">
        <xsl:param name="select" select="."/>
        <xsl:choose>
            <xsl:when test="count($select/size[@db=$db])='1'">
                <xsl:value-of select="normalize-space($select/size[@db=$db])"/>
            </xsl:when>
            <xsl:otherwise>
                <xsl:value-of select="normalize-space($select/size)"/>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>
    <!-- column ID to column name -->
    <xsl:template name="get-column-name">
        <xsl:param name="select" select="."/>

        <xsl:variable name="columns" select="key('column_id', $select)"/>
        <xsl:variable name="column" select="$columns[1]"/>
        <xsl:choose>
            <xsl:when test="count($column) = 0">
                <xsl:message terminate="yes">
                    <xsl:text>ERROR: Column with id '</xsl:text>
                    <xsl:value-of select="$select"/>
                    <xsl:text>' does not exist.</xsl:text>
                </xsl:message>
            </xsl:when>
            <xsl:otherwise>
                <xsl:call-template name="get-name">
                    <xsl:with-param name="select" select="$column"/>
                </xsl:call-template>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>
   <xsl:template name="get-column">
        <xsl:param name="id" select="/.."/>
        <xsl:variable name="columns" select="key('column_id', $id)"/>
        <xsl:variable name="column" select="$columns[1]"/>

        <xsl:choose>
            <xsl:when test="count($column) = 0">
                <xsl:message terminate="yes">
                    <xsl:text>ERROR: Column with id '</xsl:text>
                    <xsl:value-of select="$id"/>
                    <xsl:text>' does not exist.</xsl:text>
                </xsl:message>
            </xsl:when>
            <xsl:otherwise>
                <xsl:copy-of select="$column"/>
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <!-- ################ /COLUMN ################# -->

    <xsl:output method="text" indent="no" omit-xml-declaration="yes"/>
    <!-- version data template -->
    <xsl:template match="version">
	<xsl:call-template name="get-name">
	    <xsl:with-param name="select" select="parent::table"/>
	</xsl:call-template>
	<xsl:text>:</xsl:text>
	<xsl:value-of select="text()"/>
	<xsl:text>&#x0A;</xsl:text>
    </xsl:template>


    <!-- Escape all : occurrences -->
    <xsl:template name="escape">
	<xsl:param name="value"/>
	<xsl:choose>
	    <xsl:when test="contains($value, ':')">
		<xsl:value-of select="concat(substring-before($value, ':'), '\:')"/>
		<xsl:call-template name="escape">
		    <xsl:with-param name="value" select="substring-after($value, ':')"/>
		</xsl:call-template>
	    </xsl:when>
	    <xsl:otherwise>
		<xsl:value-of select="$value"/>
	    </xsl:otherwise>
	</xsl:choose>
    </xsl:template>

</xsl:stylesheet>
